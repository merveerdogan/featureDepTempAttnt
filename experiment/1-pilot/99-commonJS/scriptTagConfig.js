root = "https://perceptionexperiments.net/ME/jspsych/plugins/"



//Modified plugins
makeScriptName('me-instructions', 'me-instructions.js', root)
makeScriptName('video-resp', 'me-jspsych-video-keyboard-response.js', root)
makeScriptName('dot_disp_cl', 'special_plugins/SL_Cl/cloth_dots_display.js', root)

//Plugins  
makeScriptName('external-html', 'jspsych-external-html.js', root)
makeScriptName('preload', 'jspsych-preload.js', root)
makeScriptName('keyResp', 'jspsych-html-keyboard-response.js', root)
makeScriptName('call-function', 'jspsych-call-function.js', root)
makeScriptName('buttonResp', 'jspsych-html-button-response.js', root)
makeScriptName('survey', 'jspsych-survey-html-form.js', root)
makeScriptName('fullScreen', 'jspsych-fullscreen.js', root)
makeScriptName('slider', 'me-html-slider-response.js', root)

function makeScriptName(script_name, plugin, root) {
    source = root + plugin;
    script_name = document.createElement("script")

    //Set src attribute
    script_name.setAttribute("src", source)
    //Append to DOM
    document.head.appendChild(script_name)
}

