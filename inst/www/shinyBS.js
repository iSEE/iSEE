var shinyBS = {inputBindings: {}};

/* Got this from http://stackoverflow.com/a/3955096/3450322 */
function removeA(arr) {
    var what, a = arguments, L = a.length, ax;
    while (L > 1 && arr.length) {
        what = a[--L];
        while ((ax= arr.indexOf(what)) !== -1) {
            arr.splice(ax, 1);
        }
    }
    return arr;
}

shinyBS.inputBindings.toggle = new Shiny.InputBinding();
$.extend(shinyBS.inputBindings.toggle, {
  find: function(scope) {
    return $(scope).find(".sbs-toggle-button");
  },
  getValue: function(el) {
    return $(el).hasClass("active");
  },
  subscribe: function(el, callback) {
    $(el).on("click", function(e) {
      $(el).toggleClass("active").blur();
      callback();
    })
  },
  unsubscribe: function(el) {
    $(el).off("click");
  }
});
Shiny.inputBindings.register(shinyBS.inputBindings.toggle)

shinyBS.inputBindings.modal = new Shiny.InputBinding();
$.extend(shinyBS.inputBindings.modal, {
  find: function(scope) {
    return $(scope).find(".sbs-modal");
  },
  getValue: function(el) {
    return $(el).hasClass("in");
  },
  subscribe: function(el, callback) {
    $(el).on("hidden.bs.modal shown.bs.modal", callback)
  },
  unsubscribe: function(el) {
    $(el).off("hidden.bs.modal shown.bs.modal")
  },
  receiveMessage: function(el, data) {
    if(data.hasOwnProperty("toggle")) {
      if(data.toggle == "show") {
        $(el).modal("show");
      } else if(data.toggle == "hide") {
        $(el).modal("hide");
      } else {
        $(el).modal("toggle");
      }
    };
  },
  initialize: function(el) {
    $("#" + $(el).attr("data-sbs-trigger")).attr({"data-toggle": "modal", "data-target": "#" + $(el).attr("id")});
  }
});
Shiny.inputBindings.register(shinyBS.inputBindings.modal);

shinyBS.inputBindings.collapse = new Shiny.InputBinding();
$.extend(shinyBS.inputBindings.collapse, {
  find: function(scope) {
    return $(scope).find(".sbs-panel-group");
  },
  getValue: function(el) {
    return $(el).data("sbs-value");
  },
  receiveMessage: function(el, data) {
    var $el = $(el);
/* I would think this code should work, but it doesn't for some reason so I am 
   commenting it out.
    if(data.hasOwnProperty('multiple')) {
      if(data.multiple) {
        $el.find(".collapse").each(function(i) {$(this).collapse({parent: false, toggle: false})});
      } else {
        $el.find(".collapse").each(function(i) {$(this).collapse({parent: "#"+$el.attr("id"), toggle: false})});
      }
    }
*/
    if(data.hasOwnProperty('style')) {
      var panels = Object.keys(data.style)
      for(var i = 0; i < panels.length; i++) {
        var $p = $el.find("div[value='" + panels[i] + "']")
        $p
          .removeClass("panel-primary panel-danger panel-warning panel-error panel-info panel-success")
          .addClass("panel-" + data.style[panels[i]]);
      }
    }
    if(data.hasOwnProperty('open')) {
      if(!Array.isArray(data.open)) {
        data.open = [data.open]
      }
      data.open.forEach(function(value, index, array) {
        $el.find("div[value='" + value + "'] > .panel-collapse").collapse("show");
      })
    }
    if(data.hasOwnProperty("close")) {
      if(!Array.isArray(data.close)) {
        data.close = [data.close];
      }
      data.close.forEach(function(value, index, array) {
        $el.find("div[value='" + value + "'] > .panel-collapse").collapse("hide");
      })
    }
  },
  subscribe: function(el, callback) {
    $(el).find(".collapse").on("shown.bs.collapse hidden.bs.collapse", callback);
  },
  initialize: function(el) {
    var $el = $(el);
    var $panels = $el.children(".panel");
    var val = [];
    $panels.each(function(i) {
      if($(this).children("div.panel-collapse.collapse").hasClass("in")) {
        val.push($(this).attr("value"));
      }
      var $pan = $(this).children("div.panel-collapse.collapse");
      if($el.attr("data-sbs-multi") == "FALSE") {
        var par = "#" + $el.attr("id");
      } else {
        var par = false;
      }
      $pan.collapse({parent: par, toggle: false});
    });
    $el.data("sbs-value", val);
    $panels.on("show.bs.collapse", function(event) {
      // make sure this is not an event from a nested element
      if(!($el.is($(event.target).parent().parent()))) {
        return;
      }
      var val = $el.data("sbs-value");
      var v = $(this).attr("value");
      if(val.indexOf(v) == -1) {
        val.push(v);
      }
      $el.data("sbs-value", val)
    });
    $panels.on("hide.bs.collapse", function(event) {
      // make sure this is not an event from a nested element
      if(!($el.is($(event.target).parent().parent()))) {
        return;
      }
      var val = $el.data("sbs-value");
      var L = val.length;
      removeA(val, $(this).attr("value"))
      $el.data("sbs-value", val);
    });
  }
})
Shiny.inputBindings.register(shinyBS.inputBindings.collapse);


Shiny.addCustomMessageHandler("bsAlertCreate", function(data) {

  var create = true;
  
  if(data.hasOwnProperty("alertId")) {
    if($("#" + data.alertId).length > 0) {
      create = false;
    }
  }

  if(create) {

    var $alert = $("<div class = 'alert'></div>");
    
    if(data.hasOwnProperty('style')) {
      $alert.addClass("alert-" + data.style);
    } else {
      $alert.addClass("alert-info");
    }
    
    if(data.hasOwnProperty("dismiss")) {
      $alert.addClass("alert-dismissable");
    }
  
    if(data.hasOwnProperty("alertId")) {
      $alert.attr("id", data.alertId);
    }
    
    if(data.hasOwnProperty('dismiss')) {
      if(data.dismiss == true) {
        $alert.append("<button type='button' class='close' data-dismiss='alert'>&times;</button>")
      }
    }
  
    if(data.hasOwnProperty('title')) {
      $alert.append("<h4>" + data.title + "</h4>");
    }
    
    if(data.hasOwnProperty("content")) {
      $alert.append(data.content);
    }
  
    if(data.append == true) {
      $alert.appendTo("#" + data.id);
    } else {
      $("#" + data.id).html($alert);
    }
    
  }

});

Shiny.addCustomMessageHandler("bsAlertClose", function(alertId) {
  $("#" + alertId).alert('close');
});

// The following function refer to tooltips but are used in the creation of 
// tooltips and popovers because there structure is so similar. type="popover"
// will create a popover.

shinyBS.addTooltip = function(id, type, opts) {
  var $id = shinyBS.getTooltipTarget(id);
  var dopts = {html: true};
  opts = $.extend(opts, dopts);
  
  if(type == "tooltip") {
    $id.tooltip("destroy");
    $id.tooltip(opts);
  } else if(type == "popover") {
    $id.popover("destroy");
    $id.popover(opts);
  }
  
}

shinyBS.removeTooltip = function(id, type) {
  var $id = shinyBS.getTooltipTarget(id);
  if(type == "tooltip") {
    $id.tooltip("destroy");
  } else if(type == "popover") {
    $id.popover("destroy");
  }
}

// Makes adjustments to the tooltip and popover targets for specialized 
// shiny inputs/outputs
shinyBS.getTooltipTarget = function(id) {
  
  var $id = $("#" + id);
  
  if($id.hasClass("js-range-slider")) {
    $id = $id.parent();
  } else if($id.hasClass("selectized")) {
    $id = $id.siblings("div.selectize-control")
  }

  return $id;
  
}

Shiny.addCustomMessageHandler("updateTooltipOrPopover", function(data) {
  if(data.action == "add") {
    shinyBS.addTooltip(data.id, data.type, data.options);
  } else if(data.action == "remove") {
    shinyBS.removeTooltip(data.id, data.type)
  }
})

Shiny.addCustomMessageHandler("bsButtonUpdate", function(data) {
  
  var btn = $("button#" + data.id);
  var ico = btn.find("i");
  
  if(ico.length > 0) {
    ico = ico[0].outerHTML;
  } else {
    ico = "";
  };
  
  if(data.hasOwnProperty("label")) {
    btn.html(ico + data.label);
  };
  
  if(data.hasOwnProperty("icon")) {
    var ch = btn.children();
    if(ch.length == 0) {
      btn.prepend(data.icon);
    } else {
      btn.find("i").replaceWith(data.icon);
    };
  };
  
  if(data.hasOwnProperty("value")) {
    if(btn.hasClass("sbs-toggle-button")) {
      if(data.value != btn.hasClass("active")) {
        btn.trigger("click");
      };
    };
  };
  
  if(data.hasOwnProperty("style")) {
    btn
      .removeClass("btn-default btn-primary btn-success btn-info btn-warning btn-danger btn-link")
      .addClass("btn-" + data.style);
  };
  
  if(data.hasOwnProperty("size")) {
    btn.removeClass("btn-lg btn-sm btn-xs")
    if(data.size != "default") {
      btn.addClass(data.size);
    };
  };
  
  if(data.hasOwnProperty("block")) {
    btn.toggleClass("btn-block", data.block);
  };
  
  if(data.hasOwnProperty("disabled")) {
    if(data.disabled) {
      btn.attr("disabled", "disabled")
    } else {
      btn.attr("disabled", false)
    };
  };
  
})
