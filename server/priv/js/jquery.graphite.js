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

    var findMask = function(targets) {
        targets = targets || [];

        if (targets.length <= 1) {
            return null;
        }

        var pivot = targets[0].split(".");
        var maskIdx = -1;

        for (var i = 1; i < targets.length; i++) {
            var x = targets[i].split(".");

            if (x.length != pivot.length) {
                return null;
            }

            for (var j=0; j < pivot.length; j++) {
                if ((x[j] != pivot[j]) && (j != maskIdx)) {
                    if (-1 != maskIdx) {
                        return null;
                    }
                    maskIdx = j;
                }
            }
        }

        if (-1 != maskIdx) {
            pivot[maskIdx] = "*";
        }

        return pivot.join(".");
    };

    var truncateTargets = function(targets, threshold) {
        var length = 0;
        var result = targets.filter(function (value) {
            length += ("&target=" + value).length;
            return (length < threshold);
        });
        return result;
    };

    /* 
     * Sometimes we receive a lot of targets. We use the following optimization algorithm:
     *
     * 1. if we are able to draw all targets then draw all of them
     * 2. if we are not able to draw all target then try to find mask and draw targets by mask
     * 3. if we didn't find mask then we truncate targets
     * */

    $.fn.graphite.optimizeTargets = function(graphite, opts) {
        opts = $.extend({threshold: 3800, legendMax: 10}, opts);

        var targets = graphite.target || [];

        if (targets.length <= opts.legendMax) {
            return $.extend(graphite, {target: targets});
        };

        var mask = findMask(targets);
        if (null != mask) {
            targets = [
                'alias(lineWidth('+mask+',0.5),"")',
                'color(lineWidth(averageSeries('+mask+'),2),"ffb03b")'
            ];
            return $.extend(graphite, {target: targets, hideLegend: "0"});
        }

        return $.extend(graphite, {target: targest});
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
