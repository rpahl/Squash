shinyjs.backgroundCol = function(params) {
    var defaultParams = {
        id : null,
        col : "white"
    };
    params = shinyjs.getParams(params, defaultParams);

    var el = $("#" + params.id);
    el.css("background-color", params.col);
}

