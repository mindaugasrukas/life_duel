// Hangman game logic
const words = [
    "ACROBAT",
    "ALLIGATOR",
    "APPLE",
    "APRIL",
    "AQUARIUM",
    "ARMADILLO",
    "ATTIC",
    "AUGUST",
    "AURORA",
    "AUTUMN",
    "BACKPACK",
    "BAGEL",
    "BALLERINA",
    "BALLOON",
    "BANANA",
    "BEACHBALL",
    "BEAVER",
    "BEDROOM",
    "BEETLE",
    "BICYCLE",
    "BIRTHDAY",
    "BLIZZARD",
    "BLOCKS",
    "BOARDWALK",
    "BREAD",
    "BROCCOLI",
    "BUNNY",
    "BURGER",
    "BUTTERFLY",
    "CAMEL",
    "CAMPFIRE",
    "CANDY",
    "CANYON",
    "CARROT",
    "CASTLE",
    "CEREAL",
    "CHEESECAKE",
    "CHOCOLATE",
    "CIRCUS",
    "CLIFF",
    "CLOCK",
    "CLOUD",
    "COMPASS",
    "COMPUTER",
    "COOKIE",
    "CORALREEF",
    "CRAYON",
    "CROCODILE",
    "CROWN",
    "CUPCAKE",
    "DAYLIGHT",
    "DECEMBER",
    "DESERT",
    "DOLPHIN",
    "DONKEY",
    "DOORKNOB",
    "DRAGON",
    "ECLIPSE",
    "EGGPLANT",
    "ELEPHANT",
    "EMERALD",
    "EYEBALL",
    "FAIRY",
    "FEBRUARY",
    "FERRET",
    "FESTIVAL",
    "FIREFLY",
    "FIREWORK",
    "FLAMINGO",
    "FLASHLIGHT",
    "FLOWER",
    "FOREST",
    "FOSSIL",
    "FRIDAY",
    "GALAXY",
    "GARDEN",
    "GEYSER",
    "GIRAFFE",
    "GLACIER",
    "GOLDFISH",
    "GRASSLAND",
    "GUITAR",
    "GYMNAST",
    "HAMSTER",
    "HARBOR",
    "HEDGEHOG",
    "HELMET",
    "HIPPOPOTAMUS",
    "HOLIDAY",
    "HONEY",
    "HURRICANE",
    "ICEBERG",
    "IGUANA",
    "ISLAND",
    "JAGUAR",
    "JANUARY",
    "JELLYFISH",
    "JOKER",
    "JUBILEE",
    "JUGGLER",
    "JUNGLE",
    "KANGAROO",
    "KEYCHAIN",
    "KITCHEN",
    "KITTEN",
    "KOALA",
    "LIBRARY",
    "LIFEBOAT",
    "LIFEGUARD",
    "LIGHTHOUSE",
    "LIGHTNING",
    "LIZARD",
    "LOLLIPOP",
    "LUMBERJACK",
    "MAGIC",
    "MAGICIAN",
    "MAGNOLIA",
    "MARBLE",
    "MARCH",
    "MARKER",
    "MEADOW",
    "MERMAID",
    "MICROSCOPE",
    "MILKSHAKE",
    "MIRROR",
    "MONDAY",
    "MOONLIGHT",
    "MOOSE",
    "MOTORCYCLE",
    "MOUNTAIN",
    "MUFFIN",
    "MUSEUM",
    "MUSHROOM",
    "NATURE",
    "NOTEBOOK",
    "NOVEMBER",
    "NUGGET",
    "NURSERY",
    "OASIS",
    "OCEAN",
    "OCTOBER",
    "OPOSSUM",
    "ORANGE",
    "OSTRICH",
    "OTTER",
    "PANDA",
    "PAPAYA",
    "PARADE",
    "PARROT",
    "PEACOCK",
    "PELICAN",
    "PENCIL",
    "PENGUIN",
    "PIANO",
    "PICKLE",
    "PINEAPPLE",
    "PIRATE",
    "PIZZA",
    "PLANET",
    "PLAYGROUND",
    "POPCORN",
    "PORCUPINE",
    "POTATO",
    "PRAIRIE",
    "PUMPKIN",
    "PUPPY",
    "PUZZLE",
    "RACCOON",
    "RAINBOW",
    "RAINCOAT",
    "RAINDROP",
    "RAINFOREST",
    "RAINSTORM",
    "RHINOCEROS",
    "RIVER",
    "RIVERBANK",
    "ROBOT",
    "ROCKET",
    "ROOSTER",
    "RUBBERDUCK",
    "SANDCASTLE",
    "SANDCRAB",
    "SANDDUNE",
    "SANDPIPER",
    "SANDWICH",
    "SATELLITE",
    "SATURDAY",
    "SCHOOLBUS",
    "SCISSORS",
    "SCOOTER",
    "SEAGULL",
    "SEAHORSE",
    "SEASHELL",
    "SEASHORE",
    "SEASIDE",
    "SEPTEMBER",
    "SKUNK",
    "SLIDE",
    "SNAIL",
    "SNOWBALL",
    "SNOWFLAKE",
    "SNOWMAN",
    "SNOWSTORM",
    "SPACESHIP",
    "SPELLBOOK",
    "SPRING",
    "SQUIRREL",
    "STARFISH",
    "SUBMARINE",
    "SUMMER",
    "SUNDAY",
    "SUNFLOWER",
    "SUNRISE",
    "SUNSET",
    "SUNSHINE",
    "SURFBOARD",
    "SWING",
    "TEDDY",
    "TELEPHONE",
    "TELESCOPE",
    "THUNDER",
    "THURSDAY",
    "TOOTHBRUSH",
    "TORNADO",
    "TOYBOX",
    "TREASURE",
    "TREEHOUSE",
    "TRIANGLE",
    "TRICYCLE",
    "TUESDAY",
    "TURTLE",
    "TYPEWRITER",
    "TYRANNOSAURUS",
    "UMBRELLA",
    "UNICORN",
    "UNIVERSE",
    "UTENSIL",
    "VACATION",
    "VISITOR",
    "VOICE",
    "VOLCANO",
    "WAFFLE",
    "WATERFALL",
    "WEDNESDAY",
    "WHALE",
    "WINTER",
    "WIZARD",
    "WOODPECKER",
    "ZEBRA",
    "ZIGZAG",
    "ZOMBIE",
];
const maxWrong = 6;
let answer = "";
let guessed = [];
let wrong = 0;

const wordDisplay = document.getElementById("wordDisplay");
const lettersDiv = document.getElementById("letters");
const statusDiv = document.getElementById("status");
const messageDiv = document.getElementById("message");
const playAgainBtn = document.getElementById("playAgain");
const hangmanCanvas = document.getElementById("hangmanCanvas");

function pickWord() {
    return words[Math.floor(Math.random() * words.length)];
}

function drawHangman(stage) {
    const ctx = hangmanCanvas.getContext("2d");
    ctx.clearRect(0, 0, hangmanCanvas.width, hangmanCanvas.height);
    ctx.strokeStyle = "#333";
    ctx.lineWidth = 3;
    // Gallows
    ctx.beginPath(); ctx.moveTo(30,110); ctx.lineTo(150,110); ctx.stroke(); // base
    ctx.beginPath(); ctx.moveTo(60,110); ctx.lineTo(60,20); ctx.lineTo(110,20); ctx.lineTo(110,35); ctx.stroke();
    // Hangman parts
    if(stage>0) { ctx.beginPath(); ctx.arc(110,45,10,0,2*Math.PI); ctx.stroke(); } // head
    if(stage>1) { ctx.beginPath(); ctx.moveTo(110,55); ctx.lineTo(110,85); ctx.stroke(); } // body
    if(stage>2) { ctx.beginPath(); ctx.moveTo(110,65); ctx.lineTo(95,75); ctx.stroke(); } // left arm
    if(stage>3) { ctx.beginPath(); ctx.moveTo(110,65); ctx.lineTo(125,75); ctx.stroke(); } // right arm
    if(stage>4) { ctx.beginPath(); ctx.moveTo(110,85); ctx.lineTo(95,105); ctx.stroke(); } // left leg
    if(stage>5) { ctx.beginPath(); ctx.moveTo(110,85); ctx.lineTo(125,105); ctx.stroke(); } // right leg
}

function updateDisplay() {
    let display = "";
    for(const ch of answer) {
        display += guessed.includes(ch) ? ch : "_";
    }
    wordDisplay.textContent = display.trim();
    statusDiv.textContent = `Guesses left: ${maxWrong - wrong}`;
    drawHangman(wrong);
}

function checkWin() {
    return answer.split("").every(ch => guessed.includes(ch));
}

function checkLose() {
    return wrong >= maxWrong;
}

function endGame(win) {
    messageDiv.textContent = win ? "You won!" : `You lost! The word was: ${answer}`;
    Array.from(document.getElementsByClassName("letter-btn")).forEach(btn => btn.disabled = true);
    playAgainBtn.style.display = "inline-block";
}

function guessLetter(letter, btn) {
    btn.disabled = true;
    if(answer.includes(letter)) {
        guessed.push(letter);
        updateDisplay();
        if(checkWin()) endGame(true);
    } else {
        wrong++;
        updateDisplay();
        if(checkLose()) endGame(false);
    }
}

function setupLetters() {
    lettersDiv.innerHTML = "";
    for(let i=65; i<=90; i++) {
        const letter = String.fromCharCode(i);
        const btn = document.createElement("button");
        btn.textContent = letter;
        btn.className = "letter-btn";
        btn.onclick = () => guessLetter(letter, btn);
        lettersDiv.appendChild(btn);
    }
}

function startGame() {
    answer = pickWord();
    guessed = [];
    wrong = 0;
    messageDiv.textContent = "";
    playAgainBtn.style.display = "none";
    setupLetters();
    updateDisplay();
}

playAgainBtn.onclick = startGame;
window.onload = startGame;

// Keyboard support
window.addEventListener("keydown", e => {
    if(e.key.length === 1 && /[a-zA-Z]/.test(e.key)) {
        const letter = e.key.toUpperCase();
        const btn = Array.from(document.getElementsByClassName("letter-btn")).find(b => b.textContent === letter);
        if(btn && !btn.disabled) btn.click();
    }
});
