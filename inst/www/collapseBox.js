var iseeCollapse = new Shiny.InputBinding();
$.extend(iseeCollapse, {
  find: function(scope) {
    return $(scope).find(".isee-collapse-box");
  },

  getValue: function(el) {
    if($(el).children(".panel-collapse").hasClass("in")) {
      return true;
    } else {
      return false;
    }
  },

  setValue: function(el, value) {
    if (value) {
      $(el).children(".panel-collapse").addClass("in");
    } else {
      $(el).children(".panel-collapse").removeClass("in");
    }
  },

  subscribe: function(el, callback) {
    $(el).children(".panel-collapse").on("shown.bs.collapse hidden.bs.collapse", function() {
      callback();
    });
  },

  unsubscribe: function(el) {
    $(el).children(".panel-collapse").off("shown.bs.collapse hidden.bs.collapse");
  }
});

Shiny.inputBindings.register(iseeCollapse);
