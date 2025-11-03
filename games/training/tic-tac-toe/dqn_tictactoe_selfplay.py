#!/usr/bin/env python3

import os
import json
import random
from collections import deque
import numpy as np
import torch
import torch.nn as nn
import torch.optim as optim

# ---------- Game helpers ----------
WIN_LINES = [(0,1,2),(3,4,5),(6,7,8),
             (0,3,6),(1,4,7),(2,5,8),
             (0,4,8),(2,4,6)]

# board: np.array shape(9,), values in {1,-1,0}
# return 1 if X wins, -1 if O wins, 0 otherwise
def winner(board):
    for a,b,c in WIN_LINES:
        s = board[a] + board[b] + board[c]
        if s == 3:  return  1  # X wins
        if s == -3: return -1  # O wins
    return 0

def legal_moves(board):
    return [i for i,v in enumerate(board) if v == 0]

def terminal(board):
    return winner(board) != 0 or not any(v == 0 for v in board)

# mark in {1,-1}, where 1=X, -1=O
def play(board, action, mark):
    nb = board.copy()
    nb[action] = mark
    return nb

# ---------- DQN model ----------
class QNet(nn.Module):
    def __init__(self):
        super().__init__()
        self.net = nn.Sequential(
            nn.Linear(9, 64), nn.ReLU(),
            nn.Linear(64, 64), nn.ReLU(),
            nn.Linear(64, 9)  # one Q per cell
        )
    def forward(self, x):  # x: (B,9)
        return self.net(x)

# ---------- Agent ----------
class DQNAgent:
    def __init__(self, lr=1e-4, gamma=0.99, eps_start=1.0, eps_end=0.05, eps_decay_steps=50_000):
        # For testing
        # If MPS (Apple Silicon GPU) is available, use it; otherwise, use CPU
        #self.device = torch.device("mps" if torch.backends.mps.is_available() else "cpu")

        # Force CPU as it's faster for small models
        self.device = "cpu"

        self.q = QNet().to(self.device)
        self.target = QNet().to(self.device)
        self.target.load_state_dict(self.q.state_dict())
        self.optim = optim.Adam(self.q.parameters(), lr=lr)
        self.gamma = gamma

        self.eps_start = eps_start
        self.eps_end = eps_end
        self.eps_decay_steps = eps_decay_steps
        self.steps_done = 0

        self.replay = deque(maxlen=100_000)
        self.batch_size = 256 * 4
        self.tau = 0.005  # soft target update

    def epsilon(self):
        t = self.steps_done
        frac = min(1.0, t / self.eps_decay_steps)
        return self.eps_start + (self.eps_end - self.eps_start) * frac

    # state: np.array(9,), legal: list[int]
    def select_action(self, state, legal):
        self.steps_done += 1
        if random.random() < self.epsilon():
            return random.choice(legal)
        with torch.no_grad():
            s = torch.from_numpy(state).float().unsqueeze(0).to(self.device)  # (1,9)
            qvals = self.q(s).squeeze(0).cpu().numpy()  # (9,)
        # mask illegal moves to very low
        masked = np.full_like(qvals, -1e9)
        masked[legal] = qvals[legal]
        return int(masked.argmax())

    def push(self, s, a, r, s2, done, legal_next):
        # Store legal_next so we can mask max over legal at target time
        self.replay.append((s.astype(np.float32), a, float(r), s2.astype(np.float32), done, np.array(legal_next, dtype=np.int64)))

    def train_step(self, recent_traj=None):
        # recent_traj: list of (s, a, r, s2, done, legal_next) from the most recent game
        batch = []
        if recent_traj is not None and len(recent_traj) > 0:
            batch.extend(recent_traj)
        n_needed = self.batch_size - len(batch)
        if len(self.replay) < n_needed:
            # Not enough in replay, just use what we have
            batch.extend(random.sample(self.replay, len(self.replay)))
        else:
            batch.extend(random.sample(self.replay, n_needed))
        if len(batch) == 0:
            return 0.0
        S = torch.tensor([b[0] for b in batch]).to(self.device)          # (B,9) Initial State
        A = torch.tensor([b[1] for b in batch]).to(self.device)          # (B,) Action
        R = torch.tensor([b[2] for b in batch]).to(self.device)          # (B,) Reward
        S2 = torch.tensor([b[3] for b in batch]).to(self.device)         # (B,9) After State
        D = torch.tensor([b[4] for b in batch], dtype=torch.bool).to(self.device)  # (B,) Done
        LEGAL_NEXT = [b[5] for b in batch]               # list of arrays

        # Q(s,a)
        Qsa_all = self.q(S)                                 # (B,9)
        Qsa = Qsa_all.gather(1, A.unsqueeze(1)).squeeze(1)  # (B,)

        with torch.no_grad():
            Q_next_all = self.target(S2)                 # (B,9)
            # mask illegal moves: set to -inf
            mask = torch.full_like(Q_next_all, -1e9)
            for i, legal in enumerate(LEGAL_NEXT):
                mask[i, legal] = Q_next_all[i, legal]
            Q_next_max = mask.max(dim=1).values          # (B,)
            target = R + (~D).float() * self.gamma * Q_next_max

        loss = nn.MSELoss()(Qsa, target)

        self.optim.zero_grad()
        loss.backward()
        nn.utils.clip_grad_norm_(self.q.parameters(), 1.0)
        self.optim.step()

        # soft update target
        with torch.no_grad():
            for p, tp in zip(self.q.parameters(), self.target.parameters()):
                tp.copy_(self.tau * p + (1 - self.tau) * tp)
        return float(loss.item())

# ---------- Self-play environment wrapper ----------
def canonical(board, turn_mark):
    """
    Flip perspective so the current player is always 'X'=+1.
    If it's O's turn (mark=-1), multiply the board by -1.
    """
    return board * turn_mark

def episode_self_play(agent: DQNAgent, max_turns=9):
    """
    Plays one full game where the SAME agent plays both sides.
    Perspective trick:
      - At each turn, we feed the NN the board 'as if' current player is X.
      - When we apply the action on the real board, we use real turn_mark.
    Returns the game trajectory so we can push transitions.
    """
    board = np.zeros(9, dtype=np.int8)
    turn = 1  # X starts (1), then -1, ...
    traj = []  # (s_canon, a, r, s2_canon, done, legal_next_for_canon)

    for _ in range(max_turns):
        s_canon = canonical(board, turn)  # current player's view
        legal = legal_moves(board)

        # Check for possible win move for current player
        win_move = None
        for move in legal:
            temp_board = play(board, move, turn)
            if terminal(temp_board) and winner(temp_board) == turn:
                win_move = move
                break

        a = agent.select_action(s_canon, legal)
        board = play(board, a, turn)

        if terminal(board):
            w = winner(board)
            # reward from perspective of the player who JUST played:
            r = 1.0 if w == turn else (-1.0 if (w != 0) else 0.0)
            s2_canon = canonical(board, -turn)  # next player's view (unused when done)
            traj.append((s_canon.copy(), a, r, s2_canon.copy(), True, []))
            break
        else:
            # Penalize this move if a win was possible but not taken
            reward = 0.0
            if win_move is not None and a != win_move:
                reward = -2.0  # Strong penalty for missing a win
            s2_canon = canonical(board, -turn)  # next player's perspective
            legal_next = legal_moves(board)
            traj.append((s_canon.copy(), a, reward, s2_canon.copy(), False, legal_next))
            turn *= -1

    return traj, winner(board)

# ---------- Simple evaluation vs random ----------
def random_move(board):
    lm = legal_moves(board)
    return random.choice(lm) if lm else None

def play_vs_random(agent, n=200):
    wins=draws=losses=0
    for _ in range(n):
        board = np.zeros(9, dtype=np.int8)
        turn = 1  # X starts
        while not terminal(board):
            if turn == 1:  # our agent
                s_canon = canonical(board, turn)
                a = agent.select_action(s_canon, legal_moves(board))
            else:
                a = random_move(board)
            board = play(board, a, turn)
            turn *= -1
        w = winner(board)
        if w == 1: wins += 1
        elif w == 0: draws += 1
        else: losses += 1
    return wins, draws, losses

# ---------- Train ----------
def train(episodes=50_000):
    agent = DQNAgent()
    if os.path.exists("dqn_checkpoint.pth"):
        load_checkpoint(agent)
        print("Resumed training from checkpoint.")
    elif os.path.exists("dqn_tictactoe_games.json"):
        with open("dqn_tictactoe_games.json", "r") as f:
            games = json.load(f)
            for game in games:
                add_game_to_replay(agent, game)
    for ep in range(1, episodes+1):
        traj, _ = episode_self_play(agent)
        # Always train on the most recent game steps plus random from replay
        agent.train_step(recent_traj=traj)
        # Push the recent steps to replay
        for (s,a,r,s2,done,legal_next) in traj:
            agent.push(s, a, r, s2, done, legal_next)

        # Evaluate every 1000 episodes
        if ep % 1000 == 0:
            w, d, l = play_vs_random(agent, n=1000)
            print(f"EP {ep:6d} | eps={agent.epsilon():.3f} | vs random W/D/L = {w}/{d}/{l}")
            save_checkpoint(agent)
    return agent

# ---------- Training data collection ----------
# Example: [(state, action, turn), ...]
# moves = [
#     ([0,0,0,0,0,0,0,0,0], 0, 1),      # X plays 0
#     ([1,0,0,0,0,0,0,0,0], 4, -1),  # O plays 4
#     ([1,0,0,0,-1,0,0,0,0], 8, 1),  # X plays 8
#     # ... and so on
# ]
def add_game_to_replay(agent, moves):
    for idx, (prev_board, action, turn_mark) in enumerate(moves):
        s_canon = canonical(np.array(prev_board, dtype=np.int8), turn_mark)
        # Apply the action to get the next board
        board_after = play(np.array(prev_board, dtype=np.int8), action, turn_mark)
        done = terminal(board_after)
        s2_canon = canonical(board_after, -turn_mark)
        if done:
            w = winner(board_after)
            r = 1.0 if w == turn_mark else (-1.0 if (w != 0) else 0.0)
            agent.push(np.array(s_canon, dtype=np.float32), action, r, np.array(s2_canon, dtype=np.float32), True, [])
            break
        else:
            legal_next = legal_moves(board_after)
            agent.push(np.array(s_canon, dtype=np.float32), action, 0.0, np.array(s2_canon, dtype=np.float32), False, legal_next)

def save_checkpoint(agent, filename="dqn_checkpoint.pth"):
    checkpoint = {
        "q_state_dict": agent.q.state_dict(),
        "target_state_dict": agent.target.state_dict(),
        "optimizer_state_dict": agent.optim.state_dict(),
        "replay": list(agent.replay),
        "steps_done": agent.steps_done,
    }
    torch.save(checkpoint, filename)

def load_checkpoint(agent, filename="dqn_checkpoint.pth"):
    checkpoint = torch.load(filename, map_location=agent.device, weights_only=False)
    agent.q.load_state_dict(checkpoint["q_state_dict"])
    agent.target.load_state_dict(checkpoint["target_state_dict"])
    agent.optim.load_state_dict(checkpoint["optimizer_state_dict"])
    agent.replay = deque(checkpoint["replay"], maxlen=100_000)
    agent.steps_done = checkpoint["steps_done"]

if __name__ == "__main__":
    agent = train(episodes=20_000)
    w,d,l = play_vs_random(agent, n=1000)
    print("FINAL vs random W/D/L =", w, d, l)
    torch.save(agent.q.state_dict(), "dqn_tictactoe_model.pth")

    # # To load the model later:
    # agent = DQNAgent()
    # agent.q.load_state_dict(torch.load("dqn_tictactoe_model.pth"))
    # agent.target.load_state_dict(agent.q.state_dict())

    dummy_input = torch.zeros(1, 9).to(agent.device)
    torch.onnx.export(agent.q, dummy_input, "dqn_tictactoe.onnx", input_names=['input'], output_names=['output'], opset_version=17)
    print("Exported dqn_tictactoe.onnx")
