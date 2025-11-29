/* Merve Erdogan - 11.29.25 
Feature-Dependent Temporal Attention (Color & Size) - Pilot V1.1
*/

/*
===============================================================
GENERAL INTRODUCTION PROCEDURE
=============================================================== */
/* ------Check Device (to accept only desktop)-----*/
var isMobile = navigator.userAgent.toLowerCase().match(/mobile/i),
    isTablet = navigator.userAgent.toLowerCase().match(/tablet/i),
    isAndroid = navigator.userAgent.toLowerCase().match(/android/i),
    isiPhone = navigator.userAgent.toLowerCase().match(/iphone/i),
    isiPad = navigator.userAgent.toLowerCase().match(/ipad/i);
var checkDevice = {
    type: jsPsychHtmlButtonResponse,
    choices: [' '],
    on_start: function (checkDevice) {
        if (isAndroid || isiPad || isiPhone || isMobile || isTablet) {
            checkDevice.stimulus =
                `<div style='display: inline-block; color: ` + text_color + `; margin: 0 auto; padding: 10px 200px 10px 200px; text-align: left'> Oops, it looks like you are on a tablet, phone or other mobile device.  This experiment can only be run from a computer or laptop.  Please return to Prolific and click "stop without completing."   If you feel like this message is in error, you can contact the study author at merve.erdogan@yale.edu.<br><br> Press on the "Exit" button to exit this experiment.`;
            checkDevice.choices = ["Exit"];
            checkDevice.data.mobile = true;
        } else {
            checkDevice.stimulus =
                standard_instr_style(`Please note that this experiment is only designed to work from a computer or laptop.  Please <strong> do not </strong> continue if you are using a phone, tablet, or other mobile device.  If you are on a phone, tablet or other mobile device you will need to return to Prolific and click "stop without completing".  <br> <br> If you are currently seeing this page from a laptop or computer, please click on the "Continue" button.`);
            checkDevice.choices = ["Continue"];
            checkDevice.data.mobile = false;
        }
    },
    data: { trial_category: 'introInstructions' },
    on_finish(data) {
        if (data.mobile) {
            jsPsych.endExperiment()
        }

    }
}

/* ------Open Ended Questions-----*/
var openEndedQuestionsText = [
    `We wanted to provide a heads-up that the end of the experiment will consist of an anonymous survey with multiple questions. A few questions are open-ended questions where you need to type 1-2 sentences. Sometimes participants do not like answering open-ended questions and tend to quit a survey once they see such questions. <br> <br> If a sizable number of people quit a survey halfway, the responses will no longer be useful. Our research depends on good quality responses. Thus, please make sure you do not mind open-ended questions before continuing with experiment.<br> <br> <p><i><strong>Press 'y'</strong> on your keyboard if you agree to answer the open-ended questions at the end of the experiment.</i><br>`
];

var openEndedQuestions = {
    type: jsPsychHtmlKeyboardResponse,
    stimulus: standard_instr_style(openEndedQuestionsText)[0],
    choices: ['y'],
    data: { trial_category: 'introInstructions' },
}

/* ------Enter Fullscreen-----*/
var enterFullscreenText = [
    `This experiment needs to be completed in full-screen mode. <br><br> Clicking on the "Continue" button should bring the experiment to full-screen mode.<br> (Don't worry, we'll take you out of full-screen mode when the experiment is over.)<br><br>Once you are in full-screen mode, please do not exit full-screen mode or minimize this screen until the experiment is completed.<br>(Additionally, do not press your browser's "back" button as this will end the experiment without giving you credit.)<br><br>`
];

var enterFullscreen = {
    type: jsPsychFullscreen,
    message: standard_instr_style(enterFullscreenText)[0],
    onFinish: function (data) {
        [w, h] = [window.innerWidth || document.documentElement.clientWidth || document.body.clientWidth, window.innerHeight || document.documentElement.clientHeight || document.body.clientHeight]
        screenCenter = [w / 2, h / 2]
    },
    data: { trial_category: 'introInstructions' },
}

/*
===============================================================
COLOR VISION CHECK (ISHIHARA PLATES)
=============================================================== */

var colorVisionCheckText = [
    `Before we begin the main experiment, we will check your color vision. This is a short test to make sure that you can clearly see the color changes used in the task.<br><br>` +
    `On the next pages, you will see images made up of many colored dots. In the middle of each image, there is a number formed by dots of a slightly different color.<br><br>` +
    `Your task is simple: type the number you see on the screen. If you cannot see a number, type "X".<br><br>` +
    `There will only be a few of these images, and it will take less than a minute. You should be able to correctly respond to these to be able to continue with the main experiment (and be fully paid).`
];

// var colorVisionCheck = {
//     type: jsPsychInstructions,
//     pages: standard_instr_style(colorVisionCheckText),
//     show_clickable_nav: true,
//     allow_backward: false,
//     button_delay: delay,
//     data: { trial_category: "instructions_color_vision_check" }
// };

// Note: Ishihara plate trials would need to be added separately
// Example structure:
// var ishiharaPlate1 = {
//     type: "jsPsychHtmlKeyboardResponse",
//     stimulus: "<img src='path/to/ishihara1.jpg' style='max-width: 800px; max-height: 600px;'>",
//     choices: ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'X'],
//     prompt: "<p style='color: white; font-size: 20px;'>Type the number you see (or 'X' if you cannot see a number):</p>",
//     data: { trial_category: "ishihara_plate" }
// };

/*
===============================================================
EXP-SPECIFIC INSTRUCTION TEXTS
=============================================================== */

var instrStartText = [
    `Hello! Thank you for volunteering to help out with our study. Please take a moment to adjust your seating so that you can comfortably watch the monitor and use the keyboard. Feel free to dim the lights as well.<br><br>` +
    `Close the door or do whatever is necessary to <b>minimize disturbance during the experiment</b>. Please also take a moment to silence your phone so that you are not interrupted by any messages mid-experiment. Do not switch to any other tabs or windows until you are complete.<br><br>` +
    `We will now go over the instructions. Please read these carefully, as you will not be able to complete this experiment without following them precisely.<br><br>` +
    `A "Next" button will appear at the bottom of the screen on each page. This button will be greyed out at the beginning and will be activated after a few seconds (giving you time to read the instructions on each page). Please read everything on each page carefully before clicking on the "Next" button to continue to the next page.`
];

var colorAlternationExplanationText = [
    'In this experiment, you will see a rectangle changing its color in a steady rhythm like the one below. See how it switches from blue to purple and then back to blue again, just like the regular beat of a song.<br><br>'];

var sizeAlternationExplanationText = [
    'Just like the color, the rectangle will also rhythmically alternate its size as you can see in the example below. It will grow larger and then return to its original size in a regular rhythm.<br><br>'];

var bothFeaturesAlternatingExplanationText = [
    'In the examples so far, the rectangle alternated only one feature at a time (either its color or its size) rhythmically. In the actual experiment, both the color and the size will alternate together.<br><br>' +
    `Let's see an example of this on the next page.`
];

var colorRhythmicSizeRandomExplanationText = [
    `So far, you saw both color and size alternate rhythmically. However, during the experiment, sometimes one feature will alternate rhythmically while the other alternates randomly. Random alternation means that the changes happen at unpredictable times rather than following a steady rhythm.<br><br>` +
    `In the next example, the color alternates rhythmically while the size alternates randomly. Please, pay attention how the color alternates between blue and purple with a steady beat but the shift between blue and purple does not follow a steady pattern.`
];

var sizeRhythmicColorRandomExplanationText = [
    `Here is another example. This time, the size alternates rhythmically while the color alternates randomly. Please, pay attention how the size of the rectangle gets bigger and smaller with a steady beat, while the color change between blue and purple is rather more irregular.`
];

var taskExplanationText = [
    `At the beginning of each display, you will be told which feature (color or size) to pay attention to. That is the feature you must keep track of (that feature's rhytmic changes). Whenever the instructed feature changes, you should respond by pressing the Space key. You must try to ignore the other feature, even though it may also be changing at the same time.<br><br>` +
    `If you are instructed to pay attention to the color, you should press the Space key every time the rectangle changes to purple, and you should try to ignore any size changes throughout the whole display. If you are instructed to pay attention to the size, you should press the Space key every time the rectangle grows larger, and you should try to ignore color changes.<br><br>` +
    `Because the alternations can be quite fast, you should try to press the Space key as quickly as possible when the relevant change occurs.<br><br>` +
    `Let's practice first with the color feature.`
];

var practiceColorAttendedInstructionText = [
    `Pay attention to the <b>Color</b> change (press the Space key everytime the rectangle turns purple as soon as possible) and ignore the <b>Size</b> change.<br><br>` +
    `Click on the 'Start' button to start the display. Since the display will start immediately,you should be ready to press the 'Space' key as soon as the rectangle starts alternating its color.`
];

var practiceSizeAttendedInstructionText = [
    `Pay attention to the <b>Size</b> change (press the Space key everytime the rectangle grows larger as soon as possible) and ignore the <b>Color</b> change.<br><br>` +
    `Click on the 'Start' button to start the display.`
];

var randomAlternationReminderText = [
    `So far, both features have alternated rhythmically. As explained earlier, in the experiment sometimes only the feature you are paying attention to will alternate rhythmically, while the other alternates randomly.<br><br>` +
    `On the next page you will be told which feature to pay attention to. Remember to ignore the other feature as much as possible, just as you did in the previous pages.`
];

var practiceSizeWithRandomColorInstructionText = [
    `Pay attention to the <b>Size</b> change and ignore the <b>Color</b> change.<br><br>` +
    `Click on the 'Start' button to start the display.`
];

var practiceColorWithRandomSizeInstructionText = [
    `Pay attention to the <b>Color</b> change and ignore the <b>Size</b> change.<br><br>` +
    `Click on the 'Start' button to start the display.`
];

var finalInstructionsText = [
    `That's it. You will just do this same task for about ` + estTotalRunTime + ` minutes. The rhythm of alternation will vary from one display to the next. Sometimes the color alternation will be faster than the size alternation, sometimes the size will be faster than the color.<br><br>` +
    `Regardless of these variations, your task remains the same. If you are asked to pay attention to the color, press the Space key everytime the rectangle changes to purple as soon as possible and ignore the size changes. If you are asked to pay attention to the size, press the Space key everytime the rectangle grows larger as soon as possible and ignore the color changes.<br><br>` +
    `You may find it difficult at times to ignore the other feature, and that is completely normal. The experiment is designed to be challenging in this way. Please do your best to stay focused for the whole task, as your responses are only useful if you give your full attention to the instructed feature.<br><br>` +
    `Please always use your dominant hand to press the 'Space' key. You can now start the experiment when you are ready.`
];

/*
===============================================================
INSTRUCTION PROCEDURES
=============================================================== */

// Setup instructions after fullscreen
var instrStart = generateInstructions(instrStartText, false, 'Next', 'introInstructions');

// Color alternation explanation with live example display
var colorAlternationExplanation = {
    type: instructionPractice,
    instructionText: practice_instr_style(colorAlternationExplanationText)[0],
    attendedFeature: "color",
    attendedTempo: 300,
    distractorTempo: 300,
    displayDuration: 5000,
    rectBaseSize: paramsGeneral.rectBaseSize || [paramsGeneral.baseWidth, paramsGeneral.baseHeight],
    sizeChangeRate: paramsGeneral.sizeChangeRate,
    sizeTempo: 999999, // Very slow so size doesn't change
    baseColor: paramsGeneral.baseColor,
    colorChangeRate: paramsGeneral.colorChangeRate,
    colorTempo: 300,
    responseKey: "", // No response needed for example
    button_label_next: "Next",
    button_delay: delay,
    data: { trial_category: "introInstructions" }
};

// Size alternation explanation with live example display
var sizeAlternationExplanation = {
    type: instructionPractice,
    instructionText: practice_instr_style(sizeAlternationExplanationText)[0],
    attendedFeature: "size",
    attendedTempo: 300,
    distractorTempo: 300,
    displayDuration: 5000,
    rectBaseSize: paramsGeneral.rectBaseSize || [paramsGeneral.baseWidth, paramsGeneral.baseHeight],
    sizeChangeRate: paramsGeneral.sizeChangeRate,
    sizeTempo: 300,
    baseColor: paramsGeneral.baseColor,
    colorChangeRate: paramsGeneral.colorChangeRate,
    colorTempo: 999999, // Very slow so color doesn't change
    responseKey: "", // No response needed for example
    button_label_next: "Next",
    button_delay: delay,
    data: { trial_category: "introInstructions" }
};

// Fixation cross before examples (for other uses)
// var fixationCross = {
//     type: jsPsychHtmlKeyboardResponse,
//     stimulus: "<p style='color: white; font-size: 50px; text-align: center'>+</p>",
//     choices: 'NO_KEYS',
//     response_ends_trial: false,
//     trial_duration: 350,
//     data: { trial_category: "fixation_cross" }
// };

// Both features alternating explanation
var bothFeaturesAlternatingExplanation = generateInstructions(bothFeaturesAlternatingExplanationText, false, 'Next', 'introInstructions');


// Both features alternating example demonstration
var bothFeaturesAlternatingExample = {
    type: rhytmicFeatureChangeTask,
    attendedFeature: "color",
    attendedTempo: 280,
    distractorTempo: 320,
    displayDuration: 8000,
    rectBaseSize: paramsGeneral.rectBaseSize || [paramsGeneral.baseWidth, paramsGeneral.baseHeight],
    sizeChangeRate: paramsGeneral.sizeChangeRate,
    sizeTempo: 320,
    baseColor: paramsGeneral.baseColor,
    colorChangeRate: paramsGeneral.colorChangeRate,
    colorTempo: 280,
    responseKey: " ",
    distractorIsRandom: false,
    data: { trial_category: "introInstructions" }
};

// Color rhythmic, size random explanation
var colorRhythmicSizeRandomExplanation = generateInstructions(colorRhythmicSizeRandomExplanationText, false, 'Next', 'introInstructions');

// Color rhythmic, size random example demonstration
var colorRhythmicSizeRandomExample = {
    type: rhytmicFeatureChangeTask,
    attendedFeature: "color",
    attendedTempo: 700,
    distractorTempo: 0, // Random
    displayDuration: 10000,
    rectBaseSize: paramsGeneral.rectBaseSize || [paramsGeneral.baseWidth, paramsGeneral.baseHeight],
    sizeChangeRate: paramsGeneral.sizeChangeRate,
    sizeTempo: 0, // Random
    baseColor: paramsGeneral.baseColor,
    colorChangeRate: paramsGeneral.colorChangeRate,
    colorTempo: 700,
    responseKey: " ",
    distractorIsRandom: true,
    data: { trial_category: "introInstructions" }
};

// Size rhythmic, color random explanation
var sizeRhythmicColorRandomExplanation = generateInstructions(sizeRhythmicColorRandomExplanationText, false, 'Next', 'introInstructions');

// Size rhythmic, color random example demonstration
var sizeRhythmicColorRandomExample = {
    type: rhytmicFeatureChangeTask,
    attendedFeature: "size",
    attendedTempo: 700,
    distractorTempo: 0, // Random
    displayDuration: 10000,
    rectBaseSize: paramsGeneral.rectBaseSize || [paramsGeneral.baseWidth, paramsGeneral.baseHeight],
    sizeChangeRate: paramsGeneral.sizeChangeRate,
    sizeTempo: 700,
    baseColor: paramsGeneral.baseColor,
    colorChangeRate: paramsGeneral.colorChangeRate,
    colorTempo: 0, // Random
    responseKey: " ",
    distractorIsRandom: true,
    data: { trial_category: "introInstructions" }
};

// Task explanation
var taskExplanation = generateInstructions(taskExplanationText, false, 'Next');

// Practice color attended instruction
var practiceColorAttendedInstruction = generateInstructions(practiceColorAttendedInstructionText, false, 'Start', 'introInstructions');

// Practice display - Color attended, both rhythmic
var practiceColorAttended = {
    type: rhytmicFeatureChangeTask,
    attendedFeature: "color",
    attendedTempo: 700,
    distractorTempo: 500,
    displayDuration: 20000,
    rectBaseSize: paramsGeneral.rectBaseSize || [paramsGeneral.baseWidth, paramsGeneral.baseHeight],
    sizeChangeRate: paramsGeneral.sizeChangeRate,
    sizeTempo: 500,
    baseColor: paramsGeneral.baseColor,
    colorChangeRate: paramsGeneral.colorChangeRate,
    colorTempo: 700,
    responseKey: " ",
    distractorIsRandom: false,
    data: { trial_category: "introInstructions" }
};

// Practice size attended instruction
var practiceSizeAttendedInstruction = generateInstructions(practiceSizeAttendedInstructionText, false, 'Start', 'introInstructions');

// Practice display - Size attended, both rhythmic
var practiceSizeAttended = {
    type: rhytmicFeatureChangeTask,
    attendedFeature: "size",
    attendedTempo: 700,
    distractorTempo: 500,
    displayDuration: 20000,
    rectBaseSize: paramsGeneral.rectBaseSize || [paramsGeneral.baseWidth, paramsGeneral.baseHeight],
    sizeChangeRate: paramsGeneral.sizeChangeRate,
    sizeTempo: 700,
    baseColor: paramsGeneral.baseColor,
    colorChangeRate: paramsGeneral.colorChangeRate,
    colorTempo: 500,
    responseKey: " ",
    distractorIsRandom: false,
    data: { trial_category: "introInstructions" }
};

// Random alternation reminder
var randomAlternationReminder = generateInstructions(randomAlternationReminderText, false, 'Next', 'introInstructions');

// Practice size with random color instruction
var practiceSizeWithRandomColorInstruction = generateInstructions(practiceSizeWithRandomColorInstructionText, false, 'Start', 'introInstructions');

// Practice display - Size rhythmic, color random
var practiceSizeWithRandomColor = {
    type: rhytmicFeatureChangeTask,
    attendedFeature: "size",
    attendedTempo: 700,
    distractorTempo: 0, // Random
    displayDuration: 20000,
    rectBaseSize: paramsGeneral.rectBaseSize || [paramsGeneral.baseWidth, paramsGeneral.baseHeight],
    sizeChangeRate: paramsGeneral.sizeChangeRate,
    sizeTempo: 700,
    baseColor: paramsGeneral.baseColor,
    colorChangeRate: paramsGeneral.colorChangeRate,
    colorTempo: 0, // Random
    responseKey: " ",
    distractorIsRandom: true,
    data: { trial_category: "introInstructions" }
};

// Practice color with random size instruction
var practiceColorWithRandomSizeInstruction = generateInstructions(practiceColorWithRandomSizeInstructionText, false, 'Start', 'introInstructions');

// Practice display - Color rhythmic, size random
var practiceColorWithRandomSize = {
    type: rhytmicFeatureChangeTask,
    attendedFeature: "color",
    attendedTempo: 700,
    distractorTempo: 0, // Random
    displayDuration: 20000,
    rectBaseSize: paramsGeneral.rectBaseSize || [paramsGeneral.baseWidth, paramsGeneral.baseHeight],
    sizeChangeRate: paramsGeneral.sizeChangeRate,
    sizeTempo: 0, // Random
    baseColor: paramsGeneral.baseColor,
    colorChangeRate: paramsGeneral.colorChangeRate,
    colorTempo: 700,
    responseKey: " ",
    distractorIsRandom: true,
    data: { trial_category: "introInstructions" }
};

// Final instructions - ready to start
var instrEnd = generateInstructions(finalInstructionsText, false, 'Start', 'introInstructions');

/*
===============================================================
COMBINE INTO TIMELINE
=============================================================== */

instructions = [
    // colorVisionCheck,                      // Color vision check instructions
    // ishiharaPlate1,                     // Add Ishihara plates here
    // ishiharaPlate2,                     // Add Ishihara plates here
    instrStart,                      // Setup instructions after fullscreen
    colorAlternationExplanation,            // Color alternation explanation with live example
    sizeAlternationExplanation,             // Size alternation explanation with live example
    bothFeaturesAlternatingExplanation,     // Both features alternating explanation
    //fixationCross,                          // Fixation cross before example
    bothFeaturesAlternatingExample,         // Both features alternating example demonstration
    // colorRhythmicSizeRandomExplanation,     // Color rhythmic, size random explanation
    //fixationCross,                          // Fixation cross before example
    // colorRhythmicSizeRandomExample,         // Color rhythmic, size random example demonstration
    // sizeRhythmicColorRandomExplanation,     // Size rhythmic, color random explanation
    // fixationCross,                          // Fixation cross before example
    // sizeRhythmicColorRandomExample,         // Size rhythmic, color random example demonstration
    taskExplanation,                        // Task explanation
    practiceColorAttendedInstruction,       // Practice color attended instruction
    // fixationCross,                          // Fixation cross before practice
    practiceColorAttended,                  // Practice display - Color attended, both rhythmic
    practiceSizeAttendedInstruction,        // Practice size attended instruction
    //  fixationCross,                          // Fixation cross before practice
    practiceSizeAttended,                   // Practice display - Size attended, both rhythmic
    //randomAlternationReminder,              // Random alternation reminder
    // practiceSizeWithRandomColorInstruction, // Practice size with random color instruction
    // fixationCross,                          // Fixation cross before practice
    // practiceSizeWithRandomColor,            // Practice display - Size rhythmic, color random
    // practiceColorWithRandomSizeInstruction, // Practice color with random size instruction
    // fixationCross,                          // Fixation cross before practice
    // practiceColorWithRandomSize,            // Practice display - Color rhythmic, size random
    instrEnd,                                   // Final instructions - ready to start
]

