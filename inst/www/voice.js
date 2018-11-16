var voiceShowPanelInput = "voiceShowPanel";

var initVoice = function() {
    if (annyang) {
        Shiny.onInputChange(voiceShowPanelInput, "");
        var commands = {
            'show panel *panel': function(panel) {
                Shiny.onInputChange(voiceShowPanelInput, panel);
            },
        };
        annyang.addCommands(commands);
        annyang.start();
        }
};

$(function() {
    setTimeout(initVoice, 10);
});
