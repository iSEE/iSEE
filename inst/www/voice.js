var voiceShowPanelInput = "voiceShowPanel";
var voiceHidePanelInput = "voiceHidePanel";

var voiceControlPanelInput = "voiceControlPanel";
var voiceShowActivePanelInput = "voiceShowActivePanel";

var initVoice = function() {
    if (annyang) {
        var showActivePanelCount = 0;
        var commands = {
            'show (me) (us) panel *panel': function(panel) {
                Shiny.onInputChange(voiceShowPanelInput, panel);
            },
            'hide panel *panel': function(panel) {
                Shiny.onInputChange(voiceHidePanelInput, panel);
            },
            '(take) control (of) (the) panel *panel': function(panel) {
                Shiny.onInputChange(voiceControlPanelInput, panel);
            },
            'show (me) (us) (the) active panel': function() {
                showActivePanelCount++;
                Shiny.onInputChange(voiceShowActivePanelInput, showActivePanelCount);
            },
        };
        annyang.addCommands(commands);
        annyang.start();
        }
};

$(function() {
    setTimeout(initVoice, 10);
});
