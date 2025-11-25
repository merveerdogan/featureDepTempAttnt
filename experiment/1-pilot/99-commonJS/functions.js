var prolific_PID = getURLParameter('PROLIFIC_PID');
var hitID = getURLParameter('STUDY_ID');
var assignmentID = getURLParameter('SESSION_ID');
let subj_id = prompt("If the following participant ID is not correct please enter your ID: " + prolific_PID + "", "");

if (prolific_PID != null && prolific_PID != "" && prolific_PID != 'no_query') {
    participantID = expt_name + "_" + prolific_PID;
} else {
    participantID = expt_name + "_" + subj_id.toString();
}



/*
===============================================================
ONLINE EXPERIMENT FUNCTIONS
===============================================================
*/
/* ------Check Device (to accept only desktop)-----*/
var isMobile = navigator.userAgent.toLowerCase().match(/mobile/i),
    isTablet = navigator.userAgent.toLowerCase().match(/tablet/i),
    isAndroid = navigator.userAgent.toLowerCase().match(/android/i),
    isiPhone = navigator.userAgent.toLowerCase().match(/iphone/i),
    isiPad = navigator.userAgent.toLowerCase().match(/ipad/i);
var check_device = {
    type: 'html-button-response',
    choices: [' '],
    on_start: function (check_device) {
        if (isAndroid || isiPad || isiPhone || isMobile || isTablet) {
            check_device.stimulus =
                `<div style='display: inline-block; color: ` + text_color + `; margin: 0 auto; padding: 10px 200px 10px 200px; text-align: left'> Oops, it looks like you are on a tablet, phone or other mobile device.  This experiment can only be run from a computer or laptop.  Please return to Prolific and click "stop without completing."   If you feel like this message is in error, you can contact the study author at merve.erdogan@yale.edu.<br><br> Press on the "Exit" button to exit this experiment.`;
            check_device.choices = ["Exit"];
            check_device.data.mobile = true;
        } else {
            check_device.stimulus =
                `<div style='display: inline-block; color: ` + text_color + `; margin: 0 auto; padding: 10px 200px 10px 200px; text-align: left'> Please note that this experiment is only designed to work from a computer or laptop.  Please <strong> do not </strong> continue if you are using a phone, tablet, or other mobile device.  If you are on a phone, tablet or other mobile device you will need to return to Prolific and click "stop without completing".  <br> <br> If you are currently seeing this page from a laptop or computer, please click on the "Continue" button.<br><br>`;
            check_device.choices = ["Continue"];
            check_device.data.mobile = false;
        }
    },
    data: { trial_category: 'Other' },
    on_finish(data) {
        if (data.mobile) {
            jsPsych.endExperiment()
        }

    }
}

/*------Get Browser Information-----*/
function getBrowserInfo() {
    let ua = navigator.userAgent,
        tem,
        M = ua.match(/(opera|chrome|safari|firefox|msie|trident(?=\/))\/?\s*(\d+)/i) || [];
    if (/trident/i.test(M[1])) {
        tem = /\brv[ :]+(\d+)/g.exec(ua) || [];
        return 'IE ' + (tem[1] || '');
    }
    if (M[1] === 'Chrome') {
        tem = ua.match(/\b(OPR|Edge)\/(\d+)/);
        if (tem != null) return tem.slice(1).join(' ').replace('OPR', 'Opera');
    }
    M = M[2] ? [M[1], M[2]] : [navigator.appName, navigator.appVersion, '-?'];
    if ((tem = ua.match(/version\/(\d+)/i)) != null)
        M.splice(1, 1, tem[1]);
    return {
        'browser': M[0],
        'version': M[1]
    };
};
var browserInfo = getBrowserInfo();

/*------Limit browser to Google Chrome-----*/
var limit_browser = {
    type: 'html-button-response',
    choices: [' '],
    on_start: function (check_device) {
        if (browserInfo.browser !== 'Chrome') {
            check_device.stimulus =
                `<div style='display: inline-block; color: ` + text_color + `; margin: 0 auto; padding: 10px 200px 10px 200px; text-align: left'> Oops, it looks like you open the experiment with a browser different than Google Chrome.  This experiment can only be run with Google Chrome.  Please return to Prolific and click "stop without completing."   If you feel like this message is in error, you can contact the study author at merve.erdogan@yale.edu.<br><br> Press on the "Exit" button to exit this experiment.`;
            check_device.choices = ["Exit"];
            check_device.data.mobile = true;
        } else {
            check_device.stimulus =
                `<div style='display: inline-block; color: ` + text_color + `; margin: 0 auto; padding: 10px 200px 10px 200px; text-align: left'> This experiment is only supported by Google Chrome. Please <strong> do not </strong> continue if you are using a different browser, return to Prolific and click "stop without completing".  <br> <br> If you are currently seeing this page on Google Chrome, please click on the "Continue" button.<br><br>`;
            check_device.choices = ["Continue"];
            check_device.data.mobile = false;
        }

    },
    data: { trial_category: 'Other' },
    on_finish(data) {
        if (data.mobile) {
            jsPsych.endExperiment()
        }

    }
}

function get_viewport_size() {
    var test = document.createElement("div");

    test.style.cssText = "position: fixed;top: 0;left: 0;bottom: 0;right: 0;";
    document.documentElement.insertBefore(test, document.documentElement.firstChild);

    var dims = {
        width: test.offsetWidth,
        height: test.offsetHeight
    };
    document.documentElement.removeChild(test);

    return dims;
};



/*
===============================================================
MATH FUNCTIONS
===============================================================
*/
function calculateMean(arr) {
    if (arr.length === 0) {
        return 0; // or you can choose to return NaN or throw an error
    }

    const sum = arr.reduce((acc, val) => acc + val, 0);
    return sum / arr.length;
}

// Function to calculate standard deviation
function calculateSD(values) {
    if (values.length <= 1) return 0;
    var meanVal = calculateMean(values);
    var squaredDiffs = values.map(function (val) {
        return Math.pow(val - meanVal, 2);
    });
    var avgSquaredDiff = calculateMean(squaredDiffs);
    return Math.sqrt(avgSquaredDiff);
}

function getRandomInt(max) {
    return Math.floor(Math.random() * max);
}

function round(value, decimals = 2) {
    return Math.round(value * Math.pow(10, decimals)) / Math.pow(10, decimals);
}
/*
===============================================================
GENERAL HELPER FUNCTIONS
===============================================================
*/
function getUnique(a) {
    function onlyUnique(value, index, array) {
        return array.indexOf(value) === index;
    }

    var unique = a.filter(onlyUnique);

    return unique
}

function itemExistsInArray(item, array) {
    return array.indexOf(item) !== -1;
}

function shuffle(array) {
    for (var i = array.length - 1; i > 0; i--) {
        const j = Math.floor(Math.random() * i)
        const temp = array[i]
        array[i] = array[j]
        array[j] = temp
    }
    return array;
};

function getVariableName(value) {
    for (const key in window) {
        if (window[key] === value) {
            return key;
        }
    }
    return null;
}

/*
===============================================================
STYLE FUNCTIONS
===============================================================
*/
function standard_instr_style(x) {
    x = ["<div style='display: inline-block; margin: 0 auto; color: " + text_color + "; padding: 10px 200px 10px 200px; text-align: left'>" + x + "</div>"]
    return x
}

function change_background_color(color) {
    document.body.style.backgroundColor = color;
}


/*
===============================================================
DATA HANDLING AND SAVING
===============================================================
*/
/* ------Get URL Parameter (used for Prolific ID Info)-----*/
function getURLParameter(sParam) {
    var sPageURL = window.location.search.substring(1);
    var sURLVariables = sPageURL.split('&');
    for (var i = 0; i < sURLVariables.length; i++) {
        var sParameterName = sURLVariables[i].split('=');
        if (sParameterName[0] == sParam) {
            return sParameterName[1];
        }
    }

    return 'no_query'
};

function myPostData(page, data) {
    $.ajax({
        type: "POST",
        url: page,
        data: data,
    });
}


function saveData(name, data, folder_name, callback) {
    var jsonData = {
        filename: name,
        filedata: data,
        folder_name: folder_name
    };

    var xhr = new XMLHttpRequest();
    xhr.open('POST', 'write_data.php', true);
    xhr.setRequestHeader('Content-Type', 'application/json');

    xhr.onreadystatechange = function () {
        if (xhr.readyState === XMLHttpRequest.DONE) {
            if (xhr.status === 200) {
                console.log('Data successfully saved: ' + name + ' ' + folder_name);
                if (callback && typeof callback === 'function') {
                    callback(null); // Pass null as the error argument to indicate success
                }
            } else {
                console.error('Error: ' + xhr.statusText);
                if (callback && typeof callback === 'function') {
                    callback(xhr.statusText); // Pass the error message to the callback
                }
            }
        }
    };

    xhr.send(JSON.stringify(jsonData));
}

/*
===============================================================
GENERAL INSTRUCTIONS USED IN ALL EXPERIMENTS
===============================================================
*/
/*
==========================
INTRO SECTION
==========================
*/
/*------Consent Function-----*/
var consent = {
    type: 'html-keyboard-response',
    choices: ['y'],
    stimulus: consentForm(), //this is defined at the end of this code 
    prompt: "<div style ='color: " + text_color + "'>Please open the consent form (opens in a new tab), complete it, then press the 'Y' key to continue.",
    data: { trial_category: 'Other' },

};

/*------Survey Function-----*/
var survey = {
    type: 'html-keyboard-response',
    stimulus: surveyForm(), //this is defined at the end of this code
    choices: ['y'],
    data: { trial_category: 'Other' }
}

/*------Enter Fullscreen Function-----*/
var enter_fullscreen = {
    type: 'fullscreen',
    pointer_lock: true,
    message: `<p><div style='display: inline-block; color:` + text_color + `; margin: 0 auto; padding: 10px 200px 10px 200px; text-align: left'>This experiment needs to be completed in full-screen mode. <br><br> Clicking on the "Continue" button should bring the experiment to full-screen mode.<br> (Don't worry, we'll take you out of full-screen mode when the experiment is over.)<br><br>Once you are in full-screen mode, please do not exit full-screen mode or minimize this screen until the experiment is completed.<br>(Additionally, do not press your browser's "back" button as this will end the experiment without giving you credit.)<br><br>`,
    on_finish: function (data) {
        [w, h] = [window.innerWidth || document.documentElement.clientWidth || document.body.clientWidth, window.innerHeight || document.documentElement.clientHeight || document.body.clientHeight]
        center = [w / 2, h / 2]
    },
    data: { trial_category: 'Other' },
}

/*------Exit Fullscreen Function-----*/
var exit_fullscreen = {
    type: 'fullscreen',
    fullscreen_mode: false,
    data: { trial_category: 'Other' },
    message: '<p>div<style= "color:' + text_color + '">The experiment will switch out of full-screen mode when you press the button below</p>'
}

/*------Turn off Cursor Function-----*/
var cursor_off = {
    type: 'call-function',
    func: function () {
        document.body.style.cursor = "none";
    }
}

/*------Turn on Cursor Function-----*/
var cursor_on = {
    type: 'call-function',
    func: function () {
        document.body.style.cursor = "auto";
    }
}

/*
==========================
CLOSING SECTION
==========================
*/
/*------ Text Before Demographic Questions -----*/
var finish = finishText();
var finishing = {
    type: 'instructions',
    pages: finish, //this is defined at the end of this code
    allow_keys: false,
    show_clickable_nav: true,
    button_delay: delay,
    allow_backward: false,
    data: { trial_category: 'debreif' },
    pointer_lock: false,
}

/*------ Demographic Question & Data Saving -----*/
var include = true;
var exp_complete = false;
var debreif_qs = {
    type: 'survey-html-form',
    html: debriefForm(), //this is defined at the end of this code
    required_names: ["attntion_out"],
    data: { trial_category: 'debreif' },
    on_start: function () {
        document.body.style.backgroundColor = 'lightgray'
        document.documentElement.style.overflow = 'scroll';
        document.body.style.overflow = 'scroll';

    },
    on_finish: function (data) {
        /*------ Check if Attention Score is less than 70 -----*/
        if (data.response.attention < 70) {
            participantID = "lowAttention_" + participantID;
            include = false;
        }

        /*------ Save Data -----*/
        exp_complete = true;
        experiment_end_time = new Date();
        if (typeof experiment_start_time === 'undefined') {
            experiment_start_time = [];
        }

        jsPsych.data.addProperties({
            gender: data.response.gender,
            age: data.response.age,
            attentionScore: data.response.attention,
            participantID: participantID,
            experiment_start_time: experiment_start_time,
            experiment_end_time: experiment_end_time,
            browser: browserInfo,
        });

        var full = jsPsych.data.get();
        saveData('full_' + participantID, full.csv(), save_folder_full);

        var test_data = jsPsych.data.get().filter({ trial_category: 'test' });
        saveData('filtered_' + participantID, test_data.csv(), save_folder_filtered);

    }
}

/*------ Closing Text with Completion Code -----*/
var closing = {
    type: 'html-keyboard-response',
    stimulus: closingText(), //this is defined at the end of this code
    choices: jsPsych.NO_KEYS,
    delay: 1000,
};


/*
==============================
ALL INSTRUCTION TEXT FUNCTIONS
==============================
*/
function surveyForm() {
    var surveyContent =
        "<div style='width: 100%; color: " + text_color + "; text-align: center'><div style='display: inline-block; margin: 0 auto; padding: 10px 200px 10px 200px; text-align: left'>" +
        "<p>We wanted to provide a heads-up that the end of the experiment will consist of an anonymous survey with multiple questions. " +
        "A few questions are open-ended questions where you need to type 1-2 sentences. Sometimes participants do not like answering open-ended questions and tend to quit a survey once they see such questions. " +
        "If a sizable number of people quit a survey halfway, the responses will no longer be useful. Our research depends on good quality responses. Thus, please make sure you do not mind open-ended questions before continuing with experiment.</p>" +
        "<p><i><strong>Press 'y'</strong> on your keyboard if you agree to answer the open-ended questions at the end of the experiment.</i><br>" +
        "</div>"
    return surveyContent
};

function debriefForm() {
    var debriefContent = '<p>Finally, we just have a couple questions for you!<br>Please note that you must answer <strong>ALL</strong> the questions before clicking "Continue". ' +
        "<div style='width: 80%; text-align: left; margin: 0 auto'>" +
        '<p>Age: <br><input name="age" required type="number"  max="100" min="18" style="width: 80px; border-radius: 4px; padding: 10px 10px; margin: 8px 0; border: 1px solid #ccc; font-size: 15px" required>' +
        '<p>Please select your gender:<br><input type="radio" required id="male" name="gender" value="male"><label for="male">Male</label><br><input type="radio" id="female" name="gender" value="female"><label for="female">Female</label><br><input type="radio" id="other" name="gender" value="other"><label for="other">Other</label><br><input type="radio" id="not_say" name="gender" value="Prefer not to say"><label for="not_say">Prefer not to say</label>' +
        '<p>In 1-2 sentences, what do you think this experiment was testing? <br><input name="testing" type="text" size="50" style="width: 100%;  border-radius: 4px; padding: 10px 10px; margin: 8px 0; border: 1px solid #ccc; font-size: 15px" required>' +
        '<p>Using the slider below, on a scale of 1-100 (with 1 being very distracted, and 100 being very focused), how well did you pay attention to the experiment?  (This will not affect whether you receive credit or compensation.) <br> <p style="text-align:left;">Very Distracted <span style="float:right;">Very Focused</span></p> <input type="range" value="50" min="1" max="100" name = "attention" class="slider" required oninput="this.nextElementSibling.value = this.value"> <output id = "attntion_out"  style="text-align:center; font-size: 1.6vw">_</output> </p>' + '<p>Did you find yourself using a strategy while you were doing the experiment? If yes, please describe with 1-2 sentences.<br><input name="strategy" type="text" size="50" style="width: 100%; border-radius: 4px; padding: 10px 10px; margin: 8px 0; border: 1px solid #ccc; font-size: 15px" required></p>' + '<p>Is there anything else we should know (either about you or how you did the experiment) that may have had an impact on your results? <br><input name="other" type="text" size="50" style="width: 100%; border-radius: 4px; padding: 10px 10px; margin: 8px 0; border: 1px solid #ccc; font-size: 15px" required></p>' +
        '<p> We do our best to make sure this experiment displays the same for all monitor configurations. Where there any parts of the experiment where words or text were cut off or misaligned?  If so, do you remember where? <br><input name="errors" type="text" size="50" style="width: 100%; border-radius: 4px; padding: 10px 10px; margin: 8px 0; border: 1px solid #ccc; font-size: 15px" required>' +
        "</div>"
    return debriefContent
};

function closingText() {
    var base = "Thank you so much for your contribution to science! The experiment has concluded. Please contact merve.erdogan@yale.edu if you have any further questions.";
    var addition = "<p>Here is your <b>unique</b> code: " + completion_code + "</p><p>To recieve payment for this experiment, you must take this code back to the Prolific page that directed you here. You can close the experiment by closing this page, you do not need to do anything else. If you encounter problems during this step, please contact the email above.";
    return base + addition
};

function finishText() {
    var finish = ["You finished all displays! There is one last part left. In the following page, you will see a couple of questions and then the study will reach to the end. You will see your unique code at the last page. <p>Please click on the 'Next' button to proceed to the questions. "]

    for (var i = 0; i < finish.length; i++) {
        finish[i] = "<div style='display: inline-block; margin: 0 auto; color: " + text_color + "; padding: 10px 200px 10px 200px; text-align: left'>" + finish[i] + "</div>"
    }
    return finish
}

function consentForm() {
    // If a Qualtrics link is provided in params.js (`qualtricsConsentURL`), redirect/open it and do not show inline consent text.
    var qURL = (typeof qualtricsConsentURL !== 'undefined' && qualtricsConsentURL) ? qualtricsConsentURL : null;
    if (qURL) {
        var content = "<div style='color: " + text_color + "; text-align:center; padding: 30px;'>" +
            "<p style='font-size:18px; font-weight:700;'>You will be taken to the consent form now.</p>" +
            "<p>Please complete the consent form in the new tab. After finishing, return to this page and press the 'Y' key to continue.</p>" +
            "<p>If the consent form did not open automatically, <a href='" + qURL + "' target='_blank' rel='noopener'>click here to open the consent form</a>.</p>" +
            "</div>";

        // Try to open the consent in a new tab immediately (may be blocked by popup blockers).
        content += "<script>(function(){try{var w=window.open('" + qURL + "','_blank'); if(!w){/*popup blocked*/} }catch(e){} })();</script>";
        return content;
    }

    // No Qualtrics link: show the full inline consent text as before
    var inlineConsent = "<div style='display: inline-block; color: " + text_color + "; margin: 0 auto; padding: 10px 80px 10px 80px; text-align: left'><h1>Consent for Participation in a Research Study</h1>" +
        "<h5>STUDY TITLE</h5><p>" + study_title + "</p>" +
        "<h5>RESEARCH STUDY SUMMARY, RISKS, AND BENEFITS</h5><p>Thank you for volunteering to participate in this research study. The purpose of this study is to better understand how we see and how we think. Study activities will include examining simple displays and then responding by answering questions, pressing some keys, or using a computer mouse. Because these are activities that many people already experience hundreds or thousands of times every day, there are no risks involved in this study. The study may have no benefits to you, but it may help the scientific community come to better understand how the human mind works. Taking part in this study is your choice. You can choose to take part, or you can choose not to take part in this study. You can also change your mind at any time, with no penalty.</p>" +
        "<h5>DURATION</h5><p>If you agree to take part, the study will last approximately <strong> " + String(estTotalRunTime) + " minutes</strong>.</p>" +
        "<h5>COSTS AND COMPENSATION</h5><p>There are no costs associated with participation in this study. You will receive <strong>" + String(exp_compensation) + "</strong> for participating.</p>" +
        "<h5>CONFIDENTIALITY</h5><p>No personally identifying information will be collected, so your participation will be anonymous. The survey is anonymous. We will not know your name. We will not be able to connect any identifying information to your survey answers. However, we will know your Prolific number in order to pay you for your time. Your Prolific number could possibly be connected to your public profile, which could, in theory, be searched. We want to stress that we will not be looking at anyone's public profiles. We will keep the information about your participation in this research confidential. Your data will be pooled with those from other participants, and may be included in scientific publications and uploaded to public data repositories.</p>" +
        "<h5>LEARNING MORE</h5><p>If you have questions about this study, you may contact your experimenter Merve Erdogan at merve.erdogan@yale.edu. If you have questions about your rights as a research participant, or you have complaints about this research, you can contact the Yale Institutional Review Boards at 203-785-4688 or hrpp@yale.edu.</p>" +
        "<h5>INFORMED CONSENT</h5><p>Your participation indicates that you have read and understood this consent form and the information presented and that you agree to be in this study.</p></div>";

    var content = "<div style='color: " + text_color + ";'>" +
        "<div style='text-align:center; font-weight:bold; margin-bottom:12px;'>Please read the consent information below.</div>" +
        inlineConsent +
        "</div>";

    return content;
}