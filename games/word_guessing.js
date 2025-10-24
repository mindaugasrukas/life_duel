const WORDS = [
    "aback","abase","abate","abbey","abbot","abhor","abide","abled","abler","abode","abort","about","above","abuse","abuzz","abyss","ached","aches","acids","acing","acorn","acres","acted","actor","acute","adage","adapt","added","adder","addle","adept","admit","adobe","adopt","adore","adorn","adult","advent",
    "aegis","aerie","affix","afire","afoot","afore","afoul","after","again","agape","agate","agave","agent","agile","aging","aglow","agony","agora","agree","ahead","aided","aides","ailed","aimed","aired","aisle","alack","alarm","album","alder","alert","algae","alias","alibi","alien","align",
    "alike","alive","allay","alley","allot","allow","alloy","aloft","aloha","alone","along","aloof","aloud","alpha","altar","alter","amass","amaze","amber","amble","ambit","amend","amiss","amity","among","amour","amped","ample","amply","amuck","amuse","angel","anger","angle","angry","angst","anime","anion","anise",
    "ankle","annex","annoy","annul","anode","antic","anvil","aorta","apart","aphid","aping","apnea","apple","apply","apron","aptly","arbor","arced","arena","argon","argue","arise","armed","armor","aroma","arose","array","arrow","arson","artsy","ascot","ashen","ashes","aside","asked","asker","askew","aspen","aspic",
    "assay","asset","aster","atlas","atoll","atoms","atone","attic","audio","audit","auger","aught","aunts","aural","auras","autos","avail","avers","avert","avian","avoid","await","awake","award","aware","awash","awful","awoke","axial","axiom","axles","azure","bacon","badge","badly","bagel","baggy","baked","baker",
    "bakes","balmy","banal","bandy","bangs","banjo","banks","barbs","bared","barer","bares","barge","barks","barns","baron","barre","based","baser","bases","basic","basin","basis","baste","batch","bated","baths","baton","batty","bathe","baker","bawdy","bayed","beach","beads","beady","beard","bears","beast","beats",
    "beech","beefy","beeps","beers","beets","began","begat","beget","begin","begun","beige","being","belch","belie","belle","bells","belly","below","belts","bench","bends","bendy","berry","berth","beset","bests","betel","bible","bicep","biddy","bigot","bilge","bills","binge","bingo","biome","birch","birds",
    "birth","bison","bitch","bites","black","blade","blame","bland","blank","blast","blaze","bleak","bleat","bleed","bleep","blend","bless","blimp","blind","bling","blini","blink","bliss","blitz","bloat","block","bloke","blond","blood","bloom","bloop","blown","blows","bluey","bluer","blues","bluff","blunt","blurb",
    "blurt","blush","board","boast","boats","bobby","bocce","boche","boded","bodes","bogey","boggy","bogus","boils","bolts","bonus","booby","books","boost","booth","boots","boozy","borax","bored","borer","bores","borne","bosom","bossy","botch","bough","bound","bouts","boxes","boxer","boyar","brace","braid","brain",
    "brake","brand","brash","brass","brave","bravo","brawl","brawn","bread","break","breed","briar","bribe","brick","bride","brief","brims","brine","bring","brink","briny","brisk","broad","broil","broke","brood","brook","broom","broth","brown","brows","brunt","brush","brute","buddy","budge","buffs","buggy","bugle",
    "build","built","bulbs","bulge","bulky","bulls","bully","bumps","bumpy","bunch","bunny","bunts","buoys","burly","burnt","burps","burst","bused","bushy","busts","busty","butte","butts","buxom","buyer","bylaw","cabal","cabby","cabin","cable","cacao","cache","cacti","caddy","cadet","caged","cages","cajun","caked",
    "cakes","cakey","calfs","calls","calms","calve","camps","canal","candy","canes","canon","canto","caper","capes","capon","carat","cards","cared","carer","cares","cargo","carol","carom","carry","carts","carve","cased","cases","casks","caste","casts","catch","cater","catty","caulk","cause","cavil","cease","cedar",
    "cello","cells","cents","chaff","chain","chair","chalk","champ","chant","chaos","chaps","charm","chars","chart","chase","chasm","chats","cheap","cheat","check","cheek","cheep","cheer","chefs","chess","chest","chewy","chick","chide","chief","child","chili","chill","chime","chimp","china","chins","chips","chirp",
    "chive","choir","choke","chomp","chops","chord","chore","chose","chows","chuck","chump","chunk","churn","chute","cider","cigar","cilia","cinch","circa","cited","cites","civic","civil","clack","claim","clamp","clams","clang","clank","clans","clash","clasp","class","clean","clear","cleat","cleft","clerk","click",
    "cliff","climb","cling","clink","cloak","clock","clogs","clone","close","cloth","cloud","clout","clove","clown","clubs","cluck","clued","clues","clump","clung","coach","coals","coast","coats","cocoa","codas","coded","coder","codes","coils","coins","colas","colds","coles","colic","colon","color","colts","combs",
    "comet","comfy","comic","comma","conch","condo","cones","conic","cooks","coped","copes","coral","cords","cored","corer","cores","corgi","corns","corny","corps","costs","couch","cough","could","count","coupe","coups","court","coven","cover","coves","covet","covey","cower","coyly","crabs","crack","craft","crags",
    "cramp","crane","crank","crash","crass","crate","crave","crawl","craze","crazy","creak","cream","credo","creed","creek","creep","crepe","crept","cress","crest","crews","cribs","cried","crier","cries","crime","crimp","crisp","croak","crock","crone","crony","crook","croon","crops","cross","crowd","crown","crows",
    "crude","crumb","crush","crust","crypt","cubic","cubit","cuffs","cumin","cupid","cuppa","curbs","curds","cured","curer","cures","curls","curly","curry","curse","curve","curvy","cushy","cuter","cutie","cyber","cycle","daddy","daily","dairy","daisy","dales","dally","dance","dandy","dared","dares","darks","darns",
    "darts","dated","dater","dates","datum","daubs","daunt","dawns","dazed","deals","dealt","deans","dears","debar","debit","debut","decaf","decal","decay","decks","decor","decoy","deeds","deems","deeps","deers","defer","deify","deign","deist","delay","delta","delve","demon","demur","denim","dense","dents","depot",
    "depth","derby","desks","deter","detox","deuce","devil","dials","diary","diced","dices","dicey","digit","diked","dills","dilly","dimer","dimes","dimly","dined","diner","dines","dingy","dingo","dinky","dinos","diode","dippy","dirge","dirty","disco","discs","dishy","disks","ditch","ditto","ditty","divas","dived",
    "diver","dives","divot","dizzy","docks","dodgy","dodge","doers","doggo","doggy","dogma","doing","doled","doles","dolls","dolly","dolts","domed","domes","donor","donut","doors","dopey","dorms","dosed","doses","doted","dotes","doubt","dough","douse","doves","dowdy","dowel","dower","downs","downy","dowry","doyen",
    "dozed","dozen","dozer","dozes","draft","drags","drain","drake","drama","drams","drank","drape","drawl","drawn","draws","dread","dream","dress","dried","drier","dries","drift","drill","drink","drips","drive","droll","drone","drool","droop","drops","dross","drove","drown","drugs","druid","drums","drunk","dryer",
    "duals","ducks","ducts","dudes","duels","duets","dukes","dulls","dully","dumbo","dummy","dumps","dumpy","dunce","dunes","dunks","dusks","dusky","dusts","dusty","dutch","duvet","dwarf","dweeb","dwell","dwelt","dyers","dying","eager","eagle","early","earns","earth","eased","easel","easer","eases","eaten","eater",
    "eaves","ebony","eclat","edema","edged","edger","edges","edict","edify","edits","eerie","egret","eight","eject","eking","elate","elbow","elder","elect","elegy","elide","elite","elope","elude","elves","email","embed","ember","emery","emits","emote","empty","enact","ended","endow","endue","enemy","enjoy","ennui",
    "ensue","enter","entry","equal","equip","erase","erect","ergot","erode","error","erupt","essay","ether","ethic","ethos","evade","event","every","evict","evils","evoke","exact","exalt","exams","excel","exert","exile","exist","exits","expel","extol","extra","exult","fable","faced","faces","facet","facts","faded",
    "fader","fades","faint","fairy","faith","faked","faker","fakes","falls","false","famed","fancy","fangs","fanny","farce","fared","fares","farms","fasts","fatal","fated","fates","fatty","fault","fauna","favor","fawns","faxed","faxes","fears","feast","feats","fecal","feeds","feels","feign","feint","fella","fells",
    "felon","femme","femur","fence","fends","feral","ferns","ferry","fetch","fetid","fetus","feuds","fever","fewer","fiber","fibre","ficus","field","fiend","fiery","fifth","fifty","fight","filed","filer","files","filet","fills","filly","filmy","filth","final","finch","finds","fined","finer","fines","finks","fiord",
    "fired","firer","fires","firms","first","firth","fishy","fists","fiver","fives","fixed","fixer","fixes","fizzy","fjord","flack","flags","flail","flair","flake","flaky","flame","flank","flans","flare","flash","flask","flats","flaws","fleas","fleck","fleet","flesh","flick","flier","flies","fling","flint","flirt",
    "float","flock","flood","floor","flora","floss","flour","flout","flown","flows","flubs","fluid","fluke","flume","flung","flunk","flush","flute","flyer","foals","foams","focal","focus","foggy","foist","folds","folks","folly","foray","force","fords","forge","forks","forms","forte","forth","forty","forum","found",
    "fount","fours","foxed","foxes","foyer","frail","frame","franc","frank","fraud","freak","freed","freer","frees","fresh","friar","fried","fries","frill","frisk","fritz","frock","frond","front","frost","froth","frown","froze","fruit","fugue","fully","fumes","funds","fungi","funks","funky","funny","furls","furry",
    "fused","fuses","fussy","futon","fuzzy","gabby","gable","gaffe","gaily","gains","gaits","galas","galax","galea","galls","gamed","gamer","games","gamma","gamut","gangs","gaped","gapes","gases","gassy","gated","gates","gator","gaudy","gauge","gaunt","gauze","gavel","gawky","gayer","gazed","gazer","gazes","gears",
    "gecko","geeks","geese","genie","genre","genus","germs","ghost","ghoul","giant","giddy","gifts","gilds","gills","gimpy","gipsy","girls","girth","given","giver","gives","glade","gland","glare","glass","glaze","gleam","glean","glide","glint","globe","gloom","glory","gloss","glove","glows","glyph","gnash","gnome",
    "goads","goals","goats","godly","going","golds","golfs","golly","goner","goody","goods","goofy","goose","gored","gores","gorge","gorse","gouty","gowns","grabs","grace","grade","graft","grain","grand","grant","grape","graph","grasp","grass","grate","grave","gravy","graze","great","greed","green","greet","grids",
    "grief","grill","grime","grimy","grind","gripe","grips","grist","grits","groan","groin","groom","grope","gross","group","grout","grove","growl","grown","grows","grubs","gruel","gruff","grunt","guard","guava","guess","guest","guide","guild","guile","guilt","guise","gulch","gulfs","gulls","gully","gumbo","gummy",
    "gusto","gusts","gusty","gutsy","gypsy","habit","hacks","hails","hairs","hairy","hakes","haled","hales","hallo","halos","halts","halve","hands","handy","hangs","hanks","happy","hardy","harem","hares","harks","harms","harps","harry","harsh","haste","hasty","hatch","hated","hater","hates","hauls","haunt","haven",
    "havoc","hawks","hazed","hazel","hazes","heads","heals","heaps","heard","hears","heart","heats","heave","heavy","hedge","heeds","heels","hefty","heirs","heist","helix","hello","helms","helps","hence","herbs","herds","heron","hertz","hewed","hewer","hides","highs","hiked","hiker","hikes","hills","hilly","hinds",
    "hinge","hints","hippo","hippy","hired","hires","hitch","hoard","hoary","hobby","hobos","hocks","hoist","holds","holed","holes","holly","homes","honed","hones","honey","honks","honor","hooch","hoods","hoofs","hooks","hoops","hoots","hoped","hoper","hopes","horde","horns","horse","hosts","hotel","hotly","hound",
    "hours","house","hovel","hover","howdy","howls","hubby","huffs","huger","hulks","hullo","human","humid","humor","humph","humus","hunch","hunks","hunky","hunts","hurls","hurry","hurts","husks","husky","hussy","hutch","hydra","icier","icing","icons","ideal","ideas","idiom","idiot","idled","idler","idles","idols",
    "igloo","iliac","image","imbue","impel","imply","inane","inbox","incur","index","inept","inert","infer","ingot","inlay","inlet","inner","input","inter","intro","ionic","irate","irons","irony","islet","issue","itchy","ivory","jaunt","jazzy","jeans","jeeps","jelly","jenny","jerky","jests","jetty","jewel","jiffy",
    "jihad","jilts","jimmy","jingo","jived","jives","joeys","jolly","joust","joyed","judge","juice","juicy","jumbo","jumps","junky","juror","karat","karma","kayak","kebab","kebob","kefir","ketch","keyed","khaki","kicks","kiddo","kiddy","kills","kilos","kinda","kinds","kings","kinky","kiosk","kited","kites","kitty",
    "knack","knave","knead","kneed","kneel","knees","knelt","knife","knits","knobs","knock","knoll","knots","known","knows","label","labor","laced","laces","lacks","laden","ladle","lager","laird","lakes","lamed","lamer","lamps","lance","lands","lanes","lanky","lapel","lapse","larch","large","largo","larks","larva",
    "laser","lasso","lasts","latch","later","latex","lathe","latte","laugh","lawns","layer","lazed","leach","leads","leafs","leafy","leaks","leaky","leans","leant","leaps","leapt","learn","lease","leash","least","leave","ledge","leech","leeks","leers","lefts","legal","leggy","lemma","lemon","lemur","lends","leper",
    "levee","level","lever","libel","libra","licit","liege","lifer","lifts","light","liked","liken","liker","likes","lilac","limbo","limbs","limit","limns","limos","limps","lined","linen","liner","lines","lingo","links","lions","lipid","lippy","lisle","lists","liter","lithe","lived","liven","liver","lives","livid",
    "loads","loafs","loamy","loans","loath","lobed","lobes","local","locks","locus","lodge","lofty","logic","login","loins","lolls","loner","longs","looks","looms","loons","loony","loopy","loose","loots","loped","lopes","lords","lorry","loser","loses","lotus","louse","lousy","loved","lover","loves","lower","lowly",
    "loyal","lucid","lucks","lucky","lucre","lumps","lunar","lunch","lunge","lupus","lurch","lured","lures","lurid","lurks","lusty","lying","madam","madly","mafia","magic","magma","maids","mails","maims","mains","maize","major","maker","makes","males","mamma","maned","manes","mange","mango","mangy","mania","manic",
    "manky","manly","manna","manor","manse","maple","march","mares","marge","maria","marks","marry","marsh","marts","maser","masks","mason","masse","match","mates","maths","mauve","maxim","maybe","mayor","meals","mealy","means","meant","meats","meaty","mecca","medal","media","medic","meets","melee","melts","memos",
    "mends","menus","mercy","merge","merit","merry","messy","metal","meter","metro","micro","midge","midst","might","miles","milky","mills","mimed","mimes","mince","minds","mined","miner","mines","minge","minim","minor","mints","minus","mires","mirth","missy","mists","miter","mites","mitre","mixed","mixer","mixes",
    "moans","moats","mocha","mocks","modal","model","modem","modes","mogul","moist","moles","molly","mommy","monks","moody","moons","moors","moose","moped","mopes","moral","moray","morel","mores","moron","mossy","motel","motif","motor","motto","mould","moult","mound","mount","mourn","mouse","mouth","moved","mover",
    "moves","movie","mowed","mower","mucks","mucus","muddy","mulch","mules","mummy","mumps","munch","muons","mural","murky","mused","muser","muses","mushy","music","musky","musty","muted","muter","mutes","myrrh","nabob","nacho","nadir","naive","nanny","nasal","nasty","natal","naval","nevus","newer","newly","nexus",
    "nicer","niche","niece","nifty","night","nimbi","nines","ninja","ninny","ninny","ninth","nippy","noble","nobly","nodal","noddy","nodes","nohow","noise","noisy","nomad","nonce","nooks","noose","north","nosey","notch","noted","noter","notes","nouns","novel","nudge","nuked","nukes","nurse","nutty","nylon","oaken",
    "oakum","oases","oasis","oaths","ocean","ocher","ochre","octal","occur","ocean","offal","offer","often","ogled","ogler","ogles","oiled","oiler","okapi","okays","olden","older","olive","omega","omens","omits","onion","onset","oozed","oozes","opals","opens","opera","opine","opium","optic","orate","orbed","orbit",
    "order","organ","other","otter","ought","ounce","ousts","outdo","outed","outer","outgo","ovary","ovate","ovens","overs","overt","owing","owned","owner","oxide","paced","pacer","paces","packs","paddy","padre","paean","pagan","paged","pager","pages","pails","pains","paint","pairs","paled","paler","pales","palsy",
    "palms","palmy","panda","paned","panel","panes","pangs","panic","pansy","pants","panty","papal","papas","paper","pappy","paras","parch","pared","parer","pares","parka","parks","parry","parse","parts","party","pasha","paste","pasty","patch","paths","patio","patsy","patty","pause","paved","paves","pawed","payer",
    "peace","peach","peaks","peaky","peals","pearl","pears","pecan","pecks","pedal","peeks","peels","peens","peers","pekan","pekoe","peony","perch","peril","perky","pesky","pesto","pests","petal","peter","petty","phage","phase","phone","phony","photo","piano","picks","piece","piers","piety","piggy","pikes","pilaf",
    "piled","piles","pills","pilot","pimps","pinch","pined","pines","pings","pinks","pinto","pints","piper","pipes","pipit","pique","pithy","pivot","pixel","pixie","pizza","place","plaid","plain","plait","plane","plank","plans","plant","plate","playa","plays","plaza","plead","pleas","pleat","plied","plies","plodd",
    "plonk","plops","plots","plows","ploys","pluck","plugs","plumb","plume","plump","plums","plush","poach","poems","poesy","point","poise","poked","poker","pokes","polar","poles","polka","polls","ponds","pools","poops","porch","porgy","porks","porns","pored","pores","porky","ports","posed","poser","poses","posit",
    "posse","postS","potsy","potty","pouch","pound","power","prank","prate","pratt","prawn","prays","press","price","pride","pried","pries","prime","primo","print","prior","prism","privy","prize","probe","prods","prone","prong","proof","props","proud","prove","prowl","proxy","prude","prune","psalm","pseud","psych",
    "pubic","pucks","puffs","puffy","pulps","pulpy","pulse","pumas","punch","punks","punny","punts","pupal","pupil","puppy","pured","purer","pures","purge","purse","pushy","putty","pygmy","quack","quads","quaff","quail","quake","qualm","quant","quart","quash","quasi","queen","queer","quell","query","quest","queue",
    "quick","quiet","quill","quilt","quire","quirk","quite","quota","quote","quoth","rabbi","rabid","raced","racer","races","racks","radar","radii","radio","rafts","raged","rages","raids","rails","rains","rainy","raise","rajah","raked","rakes","rally","ramen","ramps","ranch","randy","range","ranks","rapid","rarer",
    "rasae","rasps","rated","rater","rates","ratio","raves","rawer","rayon","razed","razor","reach","react","reads","ready","realm","reals","reams","reaps","rears","rearm","rebar","rebel","rebid","rebus","rebut","recap","recon","recto","recur","recut","reeds","reedy","reeks","reels","refer","refit","regal","rehab",
    "reign","reins","relax","relay","relic","remit","remix","renal","renew","repay","repel","reply","rerun","reset","resin","rests","retag","retch","retro","retry","reuse","revel","revue","rewax","rhino","rhyme","rider","rides","ridge","rifle","rifts","right","rigid","rings","rinse","riots","ripen","riper","risen",
    "riser","rises","risks","risky","rites","rival","riven","river","rivet","roach","roads","roams","roars","roast","robed","robes","robin","robot","rocks","rocky","rodeo","rogue","roles","rollS","romeo","rooks","rooms","roomy","roost","roots","roped","ropes","rosin","rotor","rouge","rough","round","rouse","route",
    "roved","rover","roves","rowdy","rowed","rower","royal","saber","sable","sacks","sacra","sails","saint","sakes","salad","sales","salon","salsa","salts","salty","salve","salvo","sandy","saner","sappy","sarge","saris","sassy","sated","sates","satin","satyr","sauce","saucy","sauna","saved","saver","saves","savvy",
    "sawed","saxes","scaff","scald","scale","scalp","scaly","scamp","scams","scant","scare","scarf","scars","scary","scats","scene","scent","scion","scoff","scold","scone","scoop","scoot","scope","score","scorn","scour","scout","scowl","scrap","scree","screw","scrub","scuba","scuff","seams","seamy","sears","seats",
    "sects","sedan","seder","seeds","seedy","seeks","seems","seine","seize","sells","semen","sends","sense","sepal","sepia","serfs","serum","serve","setup","seven","sever","sewed","sewer","shack","shade","shady","shaft","shake","shaky","shale","shall","shalt","shame","shank","shape","shard","share","shark","sharp",
    "shave","shawl","sheaf","shear","sheds","sheen","sheep","sheer","sheet","sheik","shelf","shell","shewn","shied","shier","shies","shift","shill","shine","shiny","ships","shire","shirk","shirt","shoal","shock","shoed","shoes","shone","shook","shoot","shore","short","shots","shout","shove","shown","shows","showy",
    "shred","shrew","shrub","shrug","shuck","shunt","shush","shyly","sicks","sided","sides","sidle","siege","sieve","sifts","sighS","sight","sigma","signs","silty","since","sinew","singe","sings","sinks","sinus","sires","sirup","sisal","sissy","sitar","sited","sites","sixes","sixth","sixty","sized","sizer","sizes",
    "skate","skein","skies","skiff","skill","skimp","skims","skips","skirt","skulk","skull","skunk","slack","slain","slang","slant","slaps","slash","slate","slats","slave","slaws","sleek","sleep","sleet","slept","slice","slick","slide","slime","slims","slimy","sling","slink","slobs","sloop","slope","slops","slosh",
    "sloth","slots","slows","slugs","slump","slung","slurp","slush","slyly","smack","small","smart","smash","smear","smell","smelt","smile","smirk","smite","smith","smock","smoke","smoky","smote","snack","snags","snail","snake","snaky","snaps","snare","snarl","sneak","sneer","snide","sniff","snipe","snoop","snoot",
    "snooz","snore","snort","snout","snowy","snuck","soaks","soapy","soars","sober","socks","sodas","sofas","softS","soggy","soils","solar","soled","soles","solid","solos","solve","sonar","songs","sonic","sooth","soots","sooty","soppy","sorry","sorts","souls","sound","soupS","sours","souse","south","sowed","sower",
    "space","spade","spake","spank","spans","spare","spark","spasm","spate","spats","spawn","speak","spear","speck","speed","spell","spelt","spend","spent","sperm","spewS","spice","spicy","spied","spies","spike","spiky","spill","spilt","spine","spiny","spire","spite","spits","splat","splay","split","spoil","spoke",
    "spoof","spook","spool","spoon","spore","sport","spots","spout","spray","spree","sprig","sprit","spunk","spurn","spurt","squat","squib","stack","staff","stage","stags","stain","stair","stake","stale","stalk","stall","stamp","stand","stank","stare","stark","stars","start","stash","state","stave","stead","steak",
    "steal","steam","steed","steel","steep","steer","stein","stern","stews","stick","stiff","stile","still","stilt","sting","stint","stock","stoic","stoke","stole","stomp","stone","stony","stood","stool","stoop","stops","store","stork","storm","story","stout","stove","strap","straw","stray","strew","strip","strop",
    "stuck","studS","study","stuff","stump","stung","stunk","stunt","style","suave","sucks","sudan","sugar","suing","suite","sulky","sully","sumac","sunny","sunup","super","sureS","surge","surly","sushi","swami","swamp","swarm","swash","swath","swear","sweat","sweep","sweet","swell","swept","swift","swill","swine",
    "swing","swirl","swish","swoon","swoop","sword","swore","sworn","swung","synod","table","taboo","tacit","tacky","taffy","tails","taint","taken","taker","takes","tales","talkS","tally","talon","tamed","tamer","tames","tango","tangy","tanks","taper","tapir","tardy","tared","tares","targe","tarns","tarry","tarts",
    "tasks","tasty","tatty","taunt","tawny","taxed","taxer","taxes","teach","teams","tears","teary","tease","teems","teens","teeth","tells","tempi","tempo","tempt","tenet","tenor","tense","tenth","tents","tepid","terms","terry","tests","testy","thank","thaws","theft","their","theme","there","these","theta","thick",
    "thief","thigh","thing","think","thins","third","thong","thorn","those","three","threw","throb","throw","thrum","thumb","thump","thyme","tiara","tibia","ticks","tidal","tided","tides","tiers","tiger","tight","tilde","tiled","tiler","tiles","tills","tilth","timed","timer","times","timid","tinge","tings","tinny",
    "tints","tipsy","tired","tires","titan","tithe","title","tizzy","toads","toast","today","toddy","toffs","tofuS","togas","toils","token","toles","tolls","tombs","tomes","tonal","tones","tongs","tonic","tools","toots","topaz","topic","toque","torch","torus","total","toted","totem","toter","totes","touch","tough",
    "tours","towed","towel","tower","towns","toxic","toyed","trace","track","tract","trade","trail","train","trait","tramp","trams","traps","trash","trays","tread","treat","treed","trees","trend","tress","triad","trial","tribe","trice","trick","tried","trier","tries","trill","trims","trios","tripe","trite","troll",
    "troop","trope","trots","trout","trove","truce","truck","truer","truly","trump","trunk","truss","trust","truth","tryst","tubal","tubby","tubes","tufts","tulip","tulle","tumor","tuned","tuner","tunes","tunic","turbo","turns","tusks","tutor","twain","twang","tweak","tweed","tweet","twice","twigs","twill","twine",
    "twins","twirl","twist","tying","udder","uglis","ulcer","ulnar","ultra","umbra","unbar","uncle","uncut","under","undid","undue","unfed","unfit","unify","union","unite","units","unity","unlit","unmet","unset","untie","until","unwed","unzip","upper","upset","urban","urged","urges","urine","usage","users","usher",
    "using","usual","usurp","utile","utter","vague","valet","valid","valor","value","valve","vamps","vanes","vapid","vapor","vault","vaunt","veils","veins","veldt","venom","vents","venue","verbs","verge","verse","verso","verve","vetch","vexed","vexes","vials","vibes","vicar","vices","video","views","vigor","villa",
    "vines","vinyl","viola","viper","viral","virus","visas","visit","visor","vista","vital","vivid","vixen","vocal","vodka","vogue","voice","voles","volts","vomit","voted","voter","votes","vouch","vowed","vowel","vying","wacky","wadis","wafer","wafts","waged","wager","wages","wagon","waifs","wails","wains","waist",
    "waitS","waive","waked","waken","wakes","walkS","walls","waltz","wands","waned","wanes","wanna","wanta","wards","wareS","warms","warnS","warps","warts","warty","washy","wasps","waste","watch","water","waved","waver","waves","waxed","waxen","waxes","weary","weave","wedge","weeds","weedy","weeks","weeps","weepy",
    "weigh","weird","welds","wells","welsh","wench","whack","whale","wharf","wheat","wheel","whelk","whelp","where","which","whiff","while","whims","whine","whiny","whips","whirl","whisk","white","whole","whoop","whorl","whose","widen","wider","widow","width","wield","wight","wills","wilts","wimps","wimpy","wince",
    "winch","winds","windy","wines","wings","winks","wiped","wiper","wipes","wired","wires","wised","wiser","wises","wisps","witch","witty","wives","woken","wolfs","woman","women","woody","wooed","woofs","woozy","words","wordy","works","world","worms","worry","worse","worst","worth","would","wound","woven","wowed",
    "wrack","wraps","wrath","wreak","wreck","wrens","wrest","wring","wrist","write","wrong","wrote","wrung","wryly","xenon","xylem","yacht","yanks","yards","yarns","yawls","yawns","yearn","years","yeast","yells","yelps","yenta","yerba","yokes","young","yours","youth","yucca","yummy","zebra","zesty","zilch","zincs",
    "zingy","zippy","zonal","zones","zooms",
]

const WORD_LENGTH = 5;
const MAX_ATTEMPTS = 6;
const ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';

let secretWord = '';
let attempts = 0;
let gameOver = false;

let guesses = [];
let currentGuess = '';
// Track letter states: {A: 'absent'|'present'|'correct'|undefined, ...}
let letterStates = {};

function pickWord() {
    return WORDS[Math.floor(Math.random() * WORDS.length)];
}

function renderTiles(word, feedback) {
    const row = document.createElement('div');
    row.className = 'guess-row';
    for (let i = 0; i < WORD_LENGTH; i++) {
        const tile = document.createElement('div');
        tile.className = 'tile';
        tile.textContent = word[i] || '';
        if (feedback) tile.classList.add(feedback[i]);
        row.appendChild(tile);
    }
    return row;
}

function getFeedback(guess, answer) {
    // Returns array: 'correct', 'present', 'absent'
    const feedback = Array(WORD_LENGTH).fill('absent');
    const answerArr = answer.split('');
    const guessArr = guess.split('');
    // First pass: correct
    for (let i = 0; i < WORD_LENGTH; i++) {
        if (guessArr[i] === answerArr[i]) {
            feedback[i] = 'correct';
            answerArr[i] = null;
            guessArr[i] = null;
        }
    }
    // Second pass: present
    for (let i = 0; i < WORD_LENGTH; i++) {
        if (guessArr[i] && answerArr.includes(guessArr[i])) {
            feedback[i] = 'present';
            answerArr[answerArr.indexOf(guessArr[i])] = null;
        }
    }
    return feedback;
}

function updateLetterStates(guess, feedback) {
    // Update letterStates with the best info for each letter
    for (let i = 0; i < guess.length; i++) {
        const letter = guess[i].toUpperCase();
        const state = feedback[i];
        if (state === 'correct') {
            letterStates[letter] = 'correct';
        } else if (state === 'present') {
            if (letterStates[letter] !== 'correct') letterStates[letter] = 'present';
        } else if (state === 'absent') {
            if (!letterStates[letter]) letterStates[letter] = 'absent';
        }
    }
}

function updateDisplay() {
    // Word tiles (current guess or blanks)
    const wordTiles = document.getElementById('word-tiles');
    wordTiles.innerHTML = '';
    if (!gameOver) {
        wordTiles.appendChild(renderTiles(currentGuess.padEnd(WORD_LENGTH), null));
    } else {
        wordTiles.appendChild(renderTiles(secretWord, Array(WORD_LENGTH).fill('correct')));
    }
    // Guesses
    const guessesDiv = document.getElementById('guesses');
    guessesDiv.innerHTML = '';
    for (const {word, feedback} of guesses) {
        guessesDiv.appendChild(renderTiles(word, feedback));
    }
    // Attempts
    document.getElementById('attempts').textContent = `Attempts left: ${MAX_ATTEMPTS - attempts}`;
    // Keyboard
    renderKeyboard();
}

function showStatus(msg, color) {
    const status = document.getElementById('status');
    status.textContent = msg;
    status.style.color = color || '';
}

function endGame(win) {
    gameOver = true;
    document.getElementById('submit-btn').disabled = true;
    document.getElementById('backspace-btn').disabled = true;
    document.getElementById('restart-btn').style.display = '';
    if (win) {
        showStatus('🎉 You guessed the word!');
    } else {
        showStatus(`❌ Out of attempts! The word was: ${secretWord}`);
    }
    updateDisplay();
}

function handleGuess() {
    if (gameOver) return;
    let guess = currentGuess.trim().toLowerCase();
    if (guess.length !== WORD_LENGTH) {
        showStatus(`Enter a ${WORD_LENGTH}-letter word.`, 'orange');
        return;
    }
    if (!WORDS.includes(guess)) {
        showStatus('Not in word list.', 'orange');
        return;
    }
    attempts++;
    const feedback = getFeedback(guess, secretWord);
    guesses.push({word: guess, feedback});
    updateLetterStates(guess, feedback);
    if (guess === secretWord) {
        endGame(true);
        return;
    }
    if (attempts >= MAX_ATTEMPTS) {
        endGame(false);
        return;
    }
    currentGuess = '';
    showStatus('');
    updateDisplay();
}

function handleLetter(letter) {
    if (gameOver) return;
    if (currentGuess.length < WORD_LENGTH) {
        currentGuess += letter;
        showStatus('');
        updateDisplay();
    }
}

function handleBackspace() {
    if (gameOver) return;
    currentGuess = currentGuess.slice(0, -1);
    showStatus('');
    updateDisplay();
}

function renderKeyboard() {
    const area = document.getElementById('keyboard-area');
    area.innerHTML = '';
    for (const ch of ALPHABET) {
        const btn = document.createElement('button');
        btn.textContent = ch;
        btn.className = 'letter-btn';
        btn.onclick = () => handleLetter(ch);
        btn.disabled = gameOver || currentGuess.length >= WORD_LENGTH;
        if (letterStates[ch]) {
            btn.classList.add(letterStates[ch]);
        }
        area.appendChild(btn);
    }
}

function restartGame() {
    secretWord = pickWord();
    attempts = 0;
    gameOver = false;
    guesses = [];
    currentGuess = '';
    letterStates = {};
    document.getElementById('submit-btn').disabled = false;
    document.getElementById('backspace-btn').disabled = false;
    document.getElementById('restart-btn').style.display = 'none';
    showStatus('Guess the word! You have 6 tries.');
    updateDisplay();
}

document.getElementById('submit-btn').onclick = handleGuess;
document.getElementById('backspace-btn').onclick = handleBackspace;
document.getElementById('restart-btn').onclick = restartGame;

// Optional: allow keyboard input for accessibility
document.addEventListener('keydown', function(e) {
    if (gameOver) return;
    if (e.key === 'Enter') {
        handleGuess();
    } else if (e.key === 'Backspace') {
        handleBackspace();
    } else if (/^[a-zA-Z]$/.test(e.key)) {
        handleLetter(e.key.toUpperCase());
    }
});

// Initialize
document.addEventListener('DOMContentLoaded', restartGame);
