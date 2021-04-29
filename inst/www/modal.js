var modal = new Shiny.InputBinding();

$.extend(modal, {
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

    if(data.hasOwnProperty("update")) {
      $(el).modal('handleUpdate')
    }
  },
  initialize: function(el) {
    console.log(el);

    $("#" + $(el).attr("data-sbs-trigger"))
      .attr({
        "data-toggle": "modal",
        "data-target": "#" + $(el).attr("id")
      });
  }
});

Shiny.inputBindings.register(modal);