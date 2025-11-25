// Merve Erdogan - 10.19.25
// Feature Dependent Temporal Attention - Tapping - 1 pair
/*------------------------------------------
- Rhymic feature change display
- Task: tapping the attended feature (color or size) as fast as possible
- 1 pair of features: Color and Size
------------------------------------------*/

jsPsych.plugins["rhytmicFeatureChangeTask"] = (function () {
    var plugin = {};
    /*===============================================================
                         PARAMETERS
    ================================================================*/
    plugin.info = {
        name: "rhytmicFeatureChangeTask",
        parameters: {
            /*===============================
            CONDITION PARAMETERS
            ================================*/
            attendedFeature: {
                type: jsPsych.plugins.parameterType.STRING,
                default: "color",
                pretty_name: "Attended feature",
                description: "Feature to attend to: 'color' or 'size'."
            },
            attendedTempo: {
                type: jsPsych.plugins.parameterType.INT,
                default: 300,
                pretty_name: "Attended tempo (ms)",
                description: "Interval between attended feature changes."
            },
            distractorTempo: {
                type: jsPsych.plugins.parameterType.INT,
                default: 400,
                pretty_name: "Distractor tempo (ms)",
                description: "Interval between distractor feature changes."
            },
            testedFeaturePair: {
                type: jsPsych.plugins.parameterType.STRING,
                default: "color-size", //or size-luminance, rotation-color, rotation-luminance (for now, only color-size is supported)
                pretty_name: "Tested feature pair",
                description: "Feature pair to test: 'color-size' or 'size-luminance' or 'rotation-color' or 'rotation-luminance'."
            },
            /*===============================
            TEMPO PARAMETERS
            ================================*/
            colorTempo: {
                type: jsPsych.plugins.parameterType.INT,
                default: 300,
                pretty_name: "Color change frequency (ms)",
                description: "Interval between color changes."
            },
            sizeTempo: {
                type: jsPsych.plugins.parameterType.INT,
                default: 400,
                pretty_name: "Size change frequency (ms)",
                description: "Interval between size changes."
            },
            /*===============================
            CHANGE RATE PARAMETERS
            ================================*/
            colorChangeRate: {
                type: jsPsych.plugins.parameterType.INT,
                default: 40,
                pretty_name: "Color change amount (hsl)",
                description: "Amount of color change (hsl) between base and alternate color."
            },
            sizeChangeRate: {
                type: jsPsych.plugins.parameterType.FLOAT,
                default: 1.2,
                pretty_name: "Size change rate",
                description: "Fractional increase or decrease of size."
            },
            /*===============================
            GENERAL OBJECT PARAMETERS
            ================================*/
            rectBaseSize: {
                type: jsPsych.plugins.parameterType.INT,
                default: [50, 100],
                pretty_name: "Base size (px)",
                description: "Starting rectangle size [width, heigth]."
            },
            baseColor: {
                type: jsPsych.plugins.parameterType.STRING,
                default: "hsl(210, 100%, 75%)",
                pretty_name: "Base color",
                description: "Starting rectangle color (blue)."
            },
            /*===============================
            GENERAL TRIAL PARAMETERS
            ================================*/
            responseKey: {
                type: jsPsych.plugins.parameterType.STRING,
                default: " ",
                pretty_name: "Response key",
                description: "Key to press when attending feature changes (default: space)."
            },
            displayDuration: {
                type: jsPsych.plugins.parameterType.INT,
                default: 30000,
                pretty_name: "Display duration (ms)",
                description: "Total time the display runs."
            },
        }
    };

    /*===============================================================
                        PLUGIN RUN
    ================================================================*/
    plugin.trial = function (display_element, trial) {
        /*
       ============================
       CANVAS & SCREEN SETUP
       ============================
       */
        var html = '<canvas id="myCanvas" style="position: fixed; top: 0; left: 0; z-index: 1000;"></canvas>';
        display_element.innerHTML = html;
        var canvas = document.getElementById('myCanvas');
        var ctx = canvas.getContext("2d");
        var w = (window.innerWidth || document.documentElement.clientWidth || document.body.clientWidth);
        var h = window.innerHeight || document.documentElement.clientHeight || document.body.clientHeight;
        canvas.width = w;
        canvas.height = h;
        var screenCenter = [Math.floor(w / 2), Math.floor(h / 2)];

        document.body.style.cursor = 'none'; // <-- hide the cursor
        document.body.style.overflow = 'hidden';

        /*
        ============================
        INITIAL PARAMETERS
        ============================
        */
        const startTime = performance.now();

        // Parse base color and calculate alternate color
        const hslMatch = String(trial.baseColor).match(/hsl\((\d+),\s*(\d+)%?,\s*(\d+)%?\)/);
        const baseHue = parseInt(hslMatch[1]);
        const altHue = Math.round(baseHue + trial.colorChangeRate);
        const altColor = `hsl(${altHue}, ${parseInt(hslMatch[2])}%, ${parseInt(hslMatch[3])}%)`;
        const altSize = [trial.rectBaseSize[0] * trial.sizeChangeRate, trial.rectBaseSize[1] * trial.sizeChangeRate];

        // Extract base dimensions
        const baseWidth = trial.rectBaseSize[0];
        const baseHeight = trial.rectBaseSize[1];

        // Size state
        let large = false;
        let currentRectWidth = baseWidth;
        let currentRectHeight = baseHeight;
        let lastSizeChangeTime = 0;

        // Color state
        let colorAlt = false;
        let currentColor = trial.baseColor;
        let lastColorChangeTime = 0;

        /*  
        ============================
        DATA TRACKING PARAMETERS
        ============================
        */
        let sizeChangeLog = [];
        let colorChangeLog = [];
        let keypressLog = [];


        /*===============================
        LISTENER FOR TAP RESPONSE
        ===============================*/
        document.addEventListener('keydown', tapResponse);

        /*
        ============================
        ANIMATION
        ============================
        */
        let timeElapsed = 0;
        drawFrame();
        function drawFrame() {
            const currentTime = performance.now();
            timeElapsed = currentTime - startTime;

            if (timeElapsed < trial.displayDuration) {
                // Check if it's time for size change
                if (timeElapsed - lastSizeChangeTime >= trial.sizeTempo) {
                    large = !large;
                    if (large) {
                        currentRectWidth = baseWidth * trial.sizeChangeRate;
                        currentRectHeight = baseHeight * trial.sizeChangeRate;
                    } else {
                        currentRectWidth = baseWidth;
                        currentRectHeight = baseHeight;
                    }
                    lastSizeChangeTime = timeElapsed;
                    sizeChangeLog.push({
                        timeMs: Math.round(timeElapsed),
                        rectSize: [Math.round(currentRectWidth), Math.round(currentRectHeight)]
                    });
                }

                // Check if it's time for color change
                if (timeElapsed - lastColorChangeTime >= trial.colorTempo) {
                    colorAlt = !colorAlt;
                    currentColor = colorAlt ? altColor : trial.baseColor;
                    lastColorChangeTime = timeElapsed;
                    colorChangeLog.push({
                        timeMs: Math.round(timeElapsed),
                        colorHsl: currentColor
                    });
                }

                // Draw the frame
                ctx.fillStyle = screen_color;
                ctx.fillRect(0, 0, w, h);
                drawRect(currentRectWidth, currentRectHeight, currentColor);

                // Request next frame
                requestAnimationFrame(drawFrame);
            }
            else {
                endTrial();
            }
        }

        /*
        ============================
        END TRIAL & SAVE DATA
        ============================
        */
        function endTrial() {
            // Remove keyboard listener
            document.removeEventListener('keydown', tapResponse);

            document.body.style.cursor = "default";
            display_element.innerHTML = "";

            const trial_data = {
                attendedFeature: trial.attendedFeature,
                testedFeaturePair: trial.testedFeaturePair,
                sizeTempo: trial.sizeTempo,
                colorTempo: trial.colorTempo,
                rectBaseSize: [baseWidth, baseHeight],
                sizeChangeRate: trial.sizeChangeRate,
                sizeChangeAmount: [
                    Math.round((baseWidth * trial.sizeChangeRate) - baseWidth),
                    Math.round((baseHeight * trial.sizeChangeRate) - baseHeight)
                ],
                rectChangedSize: altSize,
                baseColor: trial.baseColor,
                colorHueChangeAmount: trial.colorChangeRate,
                colorHueChangeRate: (baseHue / trial.colorChangeRate),
                colorChangedColor: altColor,
                sizeChangeLog: JSON.stringify(sizeChangeLog),
                colorChangeLog: JSON.stringify(colorChangeLog),
                keypressLog: JSON.stringify(keypressLog),
                displayDurationSet: trial.displayDuration,
                displayDurationActualized: round(timeElapsed, 2),
            };

            jsPsych.finishTrial(trial_data);
        }

        /*
        ============================
        HELPER FUNCTIONS
        ============================
        */
        function tapResponse(e) {
            if (e.key === trial.responseKey) {
                const now = performance.now();
                keypressLog.push({
                    time_ms: Math.round(now - startTime)
                });
            }
        }

        function drawRect(width, height, color) {
            ctx.fillStyle = color;
            // Ensure perfect centering by using Math.floor for pixel-perfect positioning
            const x = Math.floor(screenCenter[0] - width / 2);
            const y = Math.floor(screenCenter[1] - height / 2);
            ctx.fillRect(x, y, width, height);
        }

        function round(value, decimals = 2) {
            return Math.round(value * Math.pow(10, decimals)) / Math.pow(10, decimals);
        }

    };

    return plugin;
})();
