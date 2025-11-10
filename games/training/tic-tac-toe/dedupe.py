#!/usr/bin/env python3

import json

# Load games
with open('dqn_tictactoe_games.json', 'r') as f:
    games = json.load(f)

# Deduplicate
unique = {}
deduped_games = []
for game in games:
    key = json.dumps(game)
    if key not in unique:
        unique[key] = True
        deduped_games.append(game)

# Pretty print to new file
with open('dqn_tictactoe_games_deduped.json', 'w') as f:
    f.write("[\n")
    for game in deduped_games:
        f.write("    [\n")
        for move in game:
            f.write(f"        {json.dumps(move)}")
            if move != game[-1]:
                f.write(f",\n")
            else:
                f.write(f"\n")
        f.write("    ]")
        if game != deduped_games[-1]:
            f.write(",\n")
        else:
            f.write("\n")
    f.write("]\n")


print(f"Deduplicated {len(games)} games to {len(deduped_games)} unique games.")
