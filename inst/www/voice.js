var voiceShowPanelInput = "voiceShowPanel";
var voiceHidePanelInput = "voiceHidePanel";

var initVoice = function() {
    if (annyang) {
        var commands = {
            'show panel *panel': function(panel) {
                Shiny.onInputChange(voiceShowPanelInput, panel);
            },
            'hide panel *panel': function(panel) {
                Shiny.onInputChange(voiceHidePanelInput, panel);
            }
        };
        annyang.addCommands(commands);
        annyang.start();
        }
};

$(function() {
    setTimeout(initVoice, 10);
});
