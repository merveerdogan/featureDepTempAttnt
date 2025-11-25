// Merve Erdogan - 10.21.25
// Feature Dependent Temporal Attention - Instruction Practice
/*------------------------------------------
- Instruction practice display plugin
- Shows instruction text at top, feature change display in middle, next button at bottom
- 1 pair of features: Color and Size
------------------------------------------*/

jsPsych.plugins["instructionPractice"] = (function () {
    var plugin = {};
    /*===============================================================
                         PARAMETERS
    ===============================================================*/
    plugin.info = {
        name: "instructionPractice",
        parameters: {
            /*===============================
            INSTRUCTION TEXT PARAMETER
            ===============================*/
            instructionText: {
                type: jsPsych.plugins.parameterType.STRING,
                default: "",
                pretty_name: "Instruction text",
                description: "Text to display at the top of the instruction page."
            },
            /*===============================
            CONDITION PARAMETERS
            ===============================*/
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
            /*===============================
            TEMPO PARAMETERS
            ===============================*/
            colorTempo: {
                type: jsPsych.plugins.parameterType.INT,
                default: 300,
                pretty_name: "Color change frequency (ms)",
                description: "Interval between color changes. Set to very high number (e.g., 999999) to disable color changes."
            },
            sizeTempo: {
                type: jsPsych.plugins.parameterType.INT,
                default: 400,
                pretty_name: "Size change frequency (ms)",
                description: "Interval between size changes. Set to very high number (e.g., 999999) to disable size changes."
            },
            /*===============================
            CHANGE RATE PARAMETERS
            ===============================*/
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
            ===============================*/
            rectBaseSize: {
                type: jsPsych.plugins.parameterType.INT,
                default: [50, 100],
                pretty_name: "Base size (px)",
                description: "Starting rectangle size [width, height]."
            },
            baseColor: {
                type: jsPsych.plugins.parameterType.STRING,
                default: "hsl(210, 100%, 75%)",
                pretty_name: "Base color",
                description: "Starting rectangle color (blue)."
            },
            /*===============================
            GENERAL TRIAL PARAMETERS
            ===============================*/
            responseKey: {
                type: jsPsych.plugins.parameterType.STRING,
                default: " ",
                pretty_name: "Response key",
                description: "Key to press when attending feature changes (default: space). Set to null or empty string to disable response."
            },
            displayDuration: {
                type: jsPsych.plugins.parameterType.INT,
                default: 5000,
                pretty_name: "Display duration (ms)",
                description: "Total time the display runs before allowing next button."
            },
            button_label_next: {
                type: jsPsych.plugins.parameterType.STRING,
                default: "Next",
                pretty_name: "Next button label",
                description: "Label for the next button."
            },
            button_delay: {
                type: jsPsych.plugins.parameterType.INT,
                default: 0,
                pretty_name: "Button delay (ms)",
                description: "Delay before the next button becomes active."
            },
        }
    };

    /*===============================================================
                        PLUGIN RUN
    ===============================================================*/
    plugin.trial = function (display_element, trial) {
        /*
       ============================
       HTML STRUCTURE SETUP
       ============================
       */
        var w = (window.innerWidth || document.documentElement.clientWidth || document.body.clientWidth);
        var h = window.innerHeight || document.documentElement.clientHeight || document.body.clientHeight;

        // Create HTML structure with text at top, canvas in middle, button at bottom
        var html = `
            <div style="display: flex; flex-direction: column; height: 100vh; width: 100vw; background-color: ${screen_color};">
                <div id="instruction-text-container" style="flex: 0 0 auto; padding: 20px; color: ${text_color}; text-align: center; overflow-y: auto;">
                    ${trial.instructionText}
                </div>
                <div id="canvas-container" style="flex: 1 1 auto; position: relative; display: flex; align-items: center; justify-content: center; overflow: hidden;">
                    <canvas id="instructionCanvas" style="max-width: 100%; max-height: 100%;"></canvas>
                </div>
                <div id="button-container" style="flex: 0 0 auto; padding: 20px; text-align: center;">
                    <button id="next-button" style="padding: 10px 30px; font-size: 18px; cursor: pointer; opacity: 0.5; pointer-events: none;" disabled>${trial.button_label_next}</button>
                </div>
            </div>
        `;

        display_element.innerHTML = html;

        // Get canvas and context
        var canvas = document.getElementById('instructionCanvas');
        var ctx = canvas.getContext("2d");

        // Set canvas size to fit container
        var canvasContainer = document.getElementById('canvas-container');
        canvas.width = canvasContainer.clientWidth;
        canvas.height = canvasContainer.clientHeight;

        var screenCenter = [Math.floor(canvas.width / 2), Math.floor(canvas.height / 2)];

        // Keep cursor visible for instructions
        document.body.style.cursor = 'default';
        document.body.style.overflow = 'hidden';

        /*
        ============================
        INITIAL PARAMETERS
        ============================
        */
        const startTime = performance.now();
        let displayEnded = false;

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
        let frameLog = [];
        let sizeChangeLog = [];
        let colorChangeLog = [];
        let keypressLog = [];

        /*===============================
        LISTENER FOR TAP RESPONSE (if enabled)
        ===============================*/
        if (trial.responseKey && trial.responseKey.trim() !== "") {
            document.addEventListener('keydown', tapResponse);
        }

        /*
        ============================
        BUTTON SETUP
        ============================
        */
        var nextButton = document.getElementById('next-button');

        // Enable button after delay
        setTimeout(function () {
            if (!displayEnded) {
                nextButton.style.opacity = '1';
                nextButton.style.pointerEvents = 'auto';
                nextButton.disabled = false;
            }
        }, trial.button_delay);

        // Button click handler
        nextButton.addEventListener('click', function () {
            endTrial();
        });

        /*
        ============================
        ANIMATION
        ============================
        */
        // Start animation frame
        drawFrame();

        function drawFrame() {
            const currentTime = performance.now();
            const timeElapsed = currentTime - startTime;

            // Continue animation even after displayDuration (for continuous display)
            // Check if it's time for size change
            if (trial.sizeTempo < 999999 && timeElapsed - lastSizeChangeTime >= trial.sizeTempo) {
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
            if (trial.colorTempo < 999999 && timeElapsed - lastColorChangeTime >= trial.colorTempo) {
                colorAlt = !colorAlt;
                currentColor = colorAlt ? altColor : trial.baseColor;
                console.log(currentColor);
                lastColorChangeTime = timeElapsed;
                colorChangeLog.push({
                    timeMs: Math.round(timeElapsed),
                    colorHsl: currentColor
                });
            }

            // Draw the frame
            ctx.fillStyle = screen_color;
            ctx.fillRect(0, 0, canvas.width, canvas.height);
            drawRect(currentRectWidth, currentRectHeight, currentColor);

            // Log frame data
            frameLog.push({
                timeMs: Math.round(timeElapsed),
                rectSize: [Math.round(currentRectWidth), Math.round(currentRectHeight)],
                colorHsl: currentColor
            });

            // Continue animation until trial ends
            if (!displayEnded) {
                requestAnimationFrame(drawFrame);
            }
        }

        /*
        ============================
        END TRIAL & SAVE DATA
        ============================
        */
        function endTrial() {
            displayEnded = true;

            // Remove keyboard listener if it was added
            if (trial.responseKey && trial.responseKey.trim() !== "") {
                document.removeEventListener('keydown', tapResponse);
            }

            document.body.style.cursor = "default";
            display_element.innerHTML = "";

            const trial_data = {
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
                sizeChangeLog: sizeChangeLog,
                colorChangeLog: colorChangeLog,
                keypressLog: keypressLog,
                displayDurationSet: trial.displayDuration,
            };
            jsPsych.finishTrial(trial_data);
        }

        /*
        ============================
        HELPER FUNCTIONS
        ============================
        */
        function tapResponse(e) {
            if (trial.responseKey && e.key === trial.responseKey) {
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

        // Handle window resize
        window.addEventListener('resize', function () {
            canvas.width = canvasContainer.clientWidth;
            canvas.height = canvasContainer.clientHeight;
            screenCenter = [Math.floor(canvas.width / 2), Math.floor(canvas.height / 2)];
        });

    };

    return plugin;
})();

