var voiceShowPanelInput = "voiceShowPanel";
var voiceHidePanelInput = "voiceHidePanel";

var voiceUpdatePanelInput = "voiceUpdatePanel";
var voiceSamePanelInput = "voiceSamePanel";

var initVoice = function() {
    if (annyang) {
        var commands = {
            'show panel *panel': function(panel) {
                Shiny.onInputChange(voiceShowPanelInput, panel);
            },
            'hide panel *panel': function(panel) {
                Shiny.onInputChange(voiceHidePanelInput, panel);
            },
            'update panel *blob': function(blob) {
                Shiny.onInputChange(voiceUpdatePanelInput, blob);
            },
            'same panel *blob': function(blob) {
                Shiny.onInputChange(voiceSamePanelInput, blob);
            },
        };
        annyang.addCommands(commands);
        annyang.start();
        }
};

$(function() {
    setTimeout(initVoice, 10);
});
