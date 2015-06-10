(function ($) {
    $.fn.graphite = function (options) {
        options = options || {};
        var settings = $.extend({}, $.fn.graphite.defaults, options);

        return this.each(function () {
            var $this = $(this);
            $this.data("graphOptions", settings);
            $.fn.graphite.render($this, settings);
        });

    };

    $.fn.graphite.geturl = function($img, options) {
        var settings = $.extend({"_t": Math.random()}, $img.data("graphOptions"), options);
        var src = settings.url + "?";

        $.each(settings, function (key, value) {
            if (key === "target") {
                $.each(value, function (index, value) {
                    src += "&target=" + value;
                });
            } else if (value !== null && key !== "url") {
                src += "&" + key + "=" + value;
            }
        });

        return src.replace(/\?&/, "?");
    };

    $.fn.graphite.render = function($img, options) {
        $img.attr("src", $.fn.graphite.geturl($img, options));
        $img.attr("height", options.height);
        $img.attr("width", options.width);
    };

    $.fn.graphite.update = function($img, options) {
        options = options || {};
        $img.each(function () {
            var $this = $(this);
            var settings = $.extend({}, $this.data("graphOptions"), options);
            $this.data("graphOptions", settings);
            $.fn.graphite.render($this, settings);
        });
    };

    // Default settings
    $.fn.graphite.defaults = {
        from: "-1hour",
        height: "300",
        until: "now",
        url: "/render/",
        width: "940"
    };

}(jQuery));
