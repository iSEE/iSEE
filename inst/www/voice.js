var voiceShowPanelInput = "voiceShowPanel";
var voiceHidePanelInput = "voiceHidePanel";

var voiceColorPanelInput = "voiceColorPanel";
var voiceColorTitleInput = "voiceColorTitle";

var initVoice = function() {
    if (annyang) {
        var commands = {
            'show panel *panel': function(panel) {
                Shiny.onInputChange(voiceShowPanelInput, panel);
            },
            'hide panel *panel': function(panel) {
                Shiny.onInputChange(voiceHidePanelInput, panel);
            },
            'color panel *panel by *title': function(panel, title) {
                Shiny.onInputChange(voiceColorPanelInput, panel);
                Shiny.onInputChange(voiceColorTitleInput, title);
            }
        };
        annyang.addCommands(commands);
        annyang.start();
        }
};

$(function() {
    setTimeout(initVoice, 10);
});
