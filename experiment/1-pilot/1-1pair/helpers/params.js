/* ------------------------------------------
Merve Erdogan - 12.06.25
Feature Dependent Temporal Attention - Tapping - 1 pair
Version: p1.3
Conditions (2x6 = 12): 
- Attended feature: Color attended (Size distractor)  vs. Size attended (Color distractor)
- Tempo (target, distractor): 480, 480
- Target-distractor start time conditions: 80, 160, 240, 320, 400, 480

Parameters:
- Color change rate (hsl): 80
- Size change rate: 1.2
- Base width: 50
- Base height: 100
- Base color: "hsl(210, 100%, 75%)"
- Background color: "white"
- Display duration: 20 seconds
------------------------------------------*/


/*===============================================================
          EXPERIMENT MODES & DEBUGGING
===============================================================*/
shortVersion = false;
if (shortVersion == true) {
    blockNum = 1;
    trialPerCondNumPerBlock = 1;
    trialPerCondNumTotal = blockNum * trialPerCondNumPerBlock;
} else {
    blockNum = 2;
    trialPerCondNumPerBlock = 1;
    trialPerCondNumTotal = blockNum * trialPerCondNumPerBlock;
}

debug = false;
runIntro = true;
runInstr = true;

//instruction delays & screen locking
if (debug == false) {
    delay = true;
    lockExp = true;
} else {
    delay = false;
    lockExp = false;
}


/*===============================================================
          EXPERIMENT-SPECIFIC PARAMETERS
===============================================================*/
/*===============================
CONDITIONS
===============================*/
attendedFeatureCond = ['color', 'size'];
tempoTargetDistCond = [[480, 480]]; //target, distractor
startTimeDiffCond = [80, 160, 240, 320, 400, 480]; //target-distractor start time conditions


/*===============================
PARAMETERS
===============================*/
paramsGeneral = {
    colorChangeRate: 80, //change in hsl (hue)
    sizeChangeRate: 1.2, //change in size
    baseWidth: 50,
    baseHeight: 100,
    baseColor: "hsl(210, 100%, 50%)",
    bgColor: "white",
    displayDuration: 20000, //20 seconds,
}

/*=============================   
ONLINE EXPERIMENT PARAMETERS
===============================*/
let estTotalRunTime = 10;
let exp_compensation = "$" + (estTotalRunTime * .20).toFixed(2).toString();
let completion_code = 'C1HEMR2R';
let study_title = 'Tap with the beat!'
let qualtricsConsentURL = "https://yalesurvey.ca1.qualtrics.com/jfe/form/SV_069acy26ux5gDZ4";


exp_version = "p1.3"
// IMPORTANT: When updating version, also update CHANGELOG.md!

let exp_date = new Date().toISOString().split('T')[0];


/*============================= 
SCREEN FEATURES
===============================*/
//The coordinate for the center of the screen
var w = window.innerWidth || document.documentElement.clientWidth || document.body.clientWidth;
var h = window.innerHeight || document.documentElement.clientHeight || document.body.clientHeight;
var screenCenter = [w / 2, h / 2];
var text_color = 'black';
var screen_color = "white";
let forceFullscreen = true;
let fullscreenActive = false;



/*============================= 
CREATE CONDITIONS ARRAY
===============================*/
// Auto-discover all globals ending in "Cond" and build factorLevels
const factorLevels = {};
for (const key in window) {
    if (!Object.prototype.hasOwnProperty.call(window, key)) continue;
    if (!key.endsWith('Cond')) continue;
    const levels = window[key];
    if (Array.isArray(levels) && levels.length > 0) {
        const factorName = key.slice(0, -4); // drop 'Cond'
        factorLevels[factorName] = levels;
    }
}

const conditionCombos = createConditionCombos(factorLevels);

conditionNum = conditionCombos.length;
blockTrialNum = conditionNum * trialPerCondNumPerBlock;
totalTrialNum = blockTrialNum * blockNum;

// Build conditions ensuring equal trials per condition per block
const factorKeys = Object.keys(factorLevels);
conditions = {};

for (const key of factorKeys) conditions[key] = [];

for (let b = 0; b < blockNum; b++) {
    //each condition index repeated trialPerCondNumPerBlock times
    const idxs = [];
    for (let i = 0; i < conditionNum; i++) {
        for (let r = 0; r < trialPerCondNumPerBlock; r++) idxs.push(i);
    }
    const shuffled = shuffle(idxs);
    for (let t = 0; t < shuffled.length; t++) {
        const combo = conditionCombos[shuffled[t]];
        for (const key of factorKeys) conditions[key].push(combo[key]);
    }
}



/*============================= 
FUNCTIONS TO USE IN THIS FILE
===============================*/
function shuffle(array) {
    return array.sort(() => Math.random() - 0.5);
}

// Cartesian product of factor levels -> array of condition objects
function createConditionCombos(levelsObj) {
    const keys = Object.keys(levelsObj);
    if (keys.length === 0) return [];
    return keys.reduce((acc, key) => {
        const vals = levelsObj[key];
        const next = [];
        for (const base of acc) {
            for (const v of vals) {
                next.push({ ...base, [key]: v });
            }
        }
        return next;
    }, [{}]);
}
