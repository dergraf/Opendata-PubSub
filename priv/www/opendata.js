(function ($) {
    // register namespace
    $.extend(true, window, {
        "OpenData": {
            "WebHooks": WebHooksList
        }
    });

    function WebHooksList(options) {
        var _options;
        var _defaults = {
            wsHost: "ws://localhost:8080/pubsub"
        };
    }

})(jQuery);
