var voiceShowPanelInput = "voiceShowPanel";
var voiceHidePanelInput = "voiceHidePanel";

var voiceControlPanelInput = "voiceControlPanel";
var voiceShowActivePanelInput = "voiceShowActivePanel";

var voiceColorUsingInput = "voiceColorUsing";
var voiceColorByInput = "voiceColorBy";
var voiceReceiveFromInput = "voiceReceiveFrom";
var voiceSendToInput = "voiceSendTo";

var initVoice = function() {
    if (annyang) {
        var showActivePanelCount = 0;
        var goodBoyCount = 0;
        var commands = {
            'show (me) (us) panel *panel': function(panel) {
                Shiny.onInputChange(voiceShowPanelInput, panel);
            },
            'hide panel *panel': function(panel) {
                Shiny.onInputChange(voiceHidePanelInput, panel);
            },
            '(give me) control (of) (the) panel *panel': function(panel) {
                Shiny.onInputChange(voiceControlPanelInput, panel);
            },
            'show (me) (us) (the) active panel': function() {
                showActivePanelCount++;
                Shiny.onInputChange(voiceShowActivePanelInput, showActivePanelCount);
            },
            'colo(u)r using *choice': function(choice) {
                Shiny.onInputChange(voiceColorUsingInput, choice);
            },
            'colo(u)r by *choice': function(choice) {
                Shiny.onInputChange(voiceColorByInput, choice);
            },
            'receive(d) (selection) from (panel) *panel': function(panel) {
                Shiny.onInputChange(voiceReceiveFromInput, panel);
            },
            'send (selection) to (panel) *panel': function(panel) {
                Shiny.onInputChange(voiceSendToInput, panel);
            },
            'good boy': function() {
                goodBoyCount++;
                Shiny.onInputChange("voiceGoodBoyInput", goodBoyCount);
            }
        };
        annyang.addCommands(commands);
        annyang.start();
        }
};

$(function() {
    setTimeout(initVoice, 10);
});
