// Math Puzzles Game Logic


// Level system
let level = 1;
const LEVELS = [
    { // 1: add/sub, logic seq, up to 20
        types: ['add', 'sub', 'seq'],
        max: 20,
        multiNum: true,
        complexLogic: false
    },
    { // 2: add/sub, logic seq, up to 100
        types: ['add', 'sub', 'seq'],
        max: 100,
        multiNum: true,
        complexLogic: false
    },
    { // 3: add/sub/mul, complex logic, up to 20
        types: ['add', 'sub', 'mul', 'seq', 'complex'],
        max: 20,
        multiNum: true,
        complexLogic: true
    },
    { // 4: add/sub/mul, complex logic, up to 100
        types: ['add', 'sub', 'mul', 'seq', 'complex'],
        max: 100,
        multiNum: true,
        complexLogic: true
    },
    { // 5: add/sub/mul/div, even more complex logic, up to 20
        types: ['add', 'sub', 'mul', 'div', 'seq', 'complex2'],
        max: 20,
        multiNum: true,
        complexLogic: true
    },
    { // 6: add/sub/mul/div, even more complex logic, up to 100
        types: ['add', 'sub', 'mul', 'div', 'seq', 'complex2'],
        max: 100,
        multiNum: true,
        complexLogic: true
    }
];

function generateRandomPuzzle() {
    const config = LEVELS[level-1] || LEVELS[LEVELS.length-1];
    const type = config.types[Math.floor(Math.random() * config.types.length)];
    let q, a;
    switch (type) {
        case 'add': {
            let nums = [];
            let count = config.multiNum ? Math.floor(Math.random() * 2) + 2 : 2;
            for (let i = 0; i < count; i++) nums.push(Math.floor(Math.random() * config.max) + 1);
            q = nums.join(' + ') + ' = ?';
            a = nums.reduce((s, n) => s + n, 0).toString();
            break;
        }
        case 'sub': {
            let nums = [];
            let count = config.multiNum ? Math.floor(Math.random() * 2) + 2 : 2;
            let first = Math.floor(Math.random() * config.max) + 10;
            for (let i = 1; i < count; i++) nums.push(Math.floor(Math.random() * (first-1)) + 1);
            q = [first].concat(nums).join(' - ') + ' = ?';
            a = [first].concat(nums).reduce((s, n) => s - n).toString();
            break;
        }
        case 'mul': {
            let nums = [];
            let count = config.multiNum ? Math.floor(Math.random() * 2) + 2 : 2;
            for (let i = 0; i < count; i++) nums.push(Math.floor(Math.random() * Math.min(12, config.max)) + 2);
            q = nums.join(' × ') + ' = ?';
            a = nums.reduce((s, n) => s * n, 1).toString();
            break;
        }
        case 'div': {
            let count = config.multiNum ? Math.floor(Math.random() * 2) + 2 : 2;
            let nums = [];
            let first = Math.floor(Math.random() * Math.min(100, config.max)) + 10;
            for (let i = 1; i < count; i++) {
                let divisor = Math.floor(Math.random() * Math.min(12, config.max)) + 2;
                first = first * divisor;
                nums.push(divisor);
            }
            q = [first].concat(nums).join(' ÷ ') + ' = ?';
            a = [first].concat(nums).reduce((s, n) => s / n).toString();
            break;
        }
        case 'seq': {
            // Arithmetic sequence
            const start = Math.floor(Math.random() * 20) + 2;
            const diff = Math.floor(Math.random() * 10) + 1;
            const n = 4;
            let seq = [start];
            for (let i = 1; i < n; i++) seq.push(seq[i-1] + diff);
            q = `What is the next number? ${seq.join(', ')}, ?`;
            a = (seq[n-1] + diff).toString();
            break;
        }
        case 'complex': {
            // Simple equation: x + a = b
            const x = Math.floor(Math.random() * config.max) + 1;
            const a1 = Math.floor(Math.random() * config.max) + 1;
            const b = x + a1;
            q = `If x + ${a1} = ${b}, x = ?`;
            a = x.toString();
            break;
        }
        case 'complex2': {
            // Equation: ax + b = c
            const a1 = Math.floor(Math.random() * 5) + 2;
            const x = Math.floor(Math.random() * config.max) + 1;
            const b = Math.floor(Math.random() * config.max) + 1;
            const c = a1 * x + b;
            q = `If ${a1}x + ${b} = ${c}, x = ?`;
            a = x.toString();
            break;
        }
        default:
            q = '1 + 1 = ?';
            a = '2';
    }
    return { q, a };
}

const PUZZLE_COUNT = 10;
let puzzles = [];
function generatePuzzleSet() {
    puzzles = [];
    for (let i = 0; i < PUZZLE_COUNT; i++) {
        puzzles.push(generateRandomPuzzle());
    }
}

let current = 0;
let score = 0;
let streak = 0;
let multiplier = 1;
let maxStreak = 0;

function showPuzzle() {
    if (current >= puzzles.length) {
        let nextLevel = level < LEVELS.length ? `<button id='next-level'>Next Level</button>` : '';
        document.getElementById('puzzle-text').innerHTML = 'Game Over!<br>Final Score: ' + score + '<br>Highest Streak: ' + maxStreak + `<br>Level: ${level}` + `<br>` + nextLevel;
        document.querySelector('.input-area').style.display = 'none';
        document.getElementById('feedback').innerText = '';
        if (nextLevel) {
            setTimeout(() => {
                document.getElementById('next-level').onclick = function() {
                    level++;
                    restartGame();
                };
            }, 100);
        }
        return;
    }
    document.getElementById('puzzle-text').innerText = puzzles[current].q;
    document.getElementById('answer').value = '';
    document.getElementById('feedback').innerText = '';
    document.querySelector('.input-area').style.display = '';
}

function updateScoreboard() {
    document.getElementById('score').innerText = score;
    document.getElementById('streak').innerText = streak;
    document.getElementById('multiplier').innerText = multiplier + 'x';
}

function showFadeMessage(msg) {
    const fade = document.getElementById('fade-message');
    fade.innerText = msg;
    fade.classList.add('show');
    setTimeout(() => fade.classList.remove('show'), 1200);
}

function submitAnswer() {
    const userAns = document.getElementById('answer').value.trim();
    const correct = puzzles[current].a;
    if (userAns === correct) {
        streak++;
        if (streak > maxStreak) maxStreak = streak;
        if (streak > 1) {
            multiplier = Math.min(5, multiplier + 1);
            if (multiplier === 2) showFadeMessage('Double Score!');
            else if (multiplier === 3) showFadeMessage('Triple Score!');
            else showFadeMessage(multiplier + 'x Score!');
        } else {
            multiplier = 1;
        }
        score += 10 * multiplier;
        document.getElementById('feedback').innerText = '✅ Correct!';
        current++;
        updateScoreboard();
        setTimeout(showPuzzle, 900);
    } else {
        document.getElementById('feedback').innerText = '❌ Incorrect! Streak reset.';
        streak = 0;
        multiplier = 1;
        updateScoreboard();
    }
}

document.getElementById('submit').onclick = submitAnswer;
document.getElementById('answer').addEventListener('keydown', function(e) {
    if (e.key === 'Enter') submitAnswer();
});

function restartGame() {
    current = 0;
    score = 0;
    streak = 0;
    multiplier = 1;
    maxStreak = 0;
    generatePuzzleSet();
    updateScoreboard();
    showPuzzle();
}

window.onload = function() {
    level = 1;
    generatePuzzleSet();
    updateScoreboard();
    showPuzzle();
};
