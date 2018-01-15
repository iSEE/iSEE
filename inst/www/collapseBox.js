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
      $(el).children(".panel-collapse").classList.add("in");
    } else {
      $(el).children(".panel-collapse").classList.remove("in");
    }
  },

  subscribe: function(el, callback) {
    $(el).children(".panel-collapse").on("shown.bs.collapse hidden.bs.collapse", callback);
  },

  unsubscribe: function(el) {
    $(el).off(".iseeCollapse");
  }
});

Shiny.inputBindings.register(iseeCollapse);
