import React, { PropTypes } from 'react';

class Graph extends React.Component {
    componentDidMount() {
        this._createTimer();
    }

    componentWillUnmount() {
        if (this.timer) {
            clearTimeout(this.timer);
        }
    }

    _createTimer() {
        this.forceUpdate();
        this.timer = this.props.autoUpdate ? setTimeout(this._createTimer.bind(this), this.props.autoUpdateInterval) : undefined;
    }

    _getUrl(options) {
        let src = this.props.url + "?";
        Object.keys(options).forEach((key) => {
            let value = options[key];
            if (key == "target") {
                value.forEach((value) => {
                    src += "&target=" + value;
                });
            } else if (value !== null) {
                src += "&" + key + "=" + value;
            }
        });
        return src.replace(/\?&/, "?");
    }

    render() {
        let options = Object.assign({"_t": Math.random(), "width": 555, "height": 418}, this.props.graphiteOpts);
        let optimizedOptions = this._optimizeTargets(options);
        let bigGraphOptions = Object.assign({}, optimizedOptions, {width: 960, height: 720});

        return (
            <a href={this._getUrl(bigGraphOptions)} target="_blank">
                <img className="graph" src={this._getUrl(optimizedOptions)} width={optimizedOptions.width}/>
            </a>
        );
    }

    _findMask(targets = []) {
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
    }



    _truncateTargets(targets, threshold) {
        var length = 0;
        var result = targets.filter(function (value) {
            length += ("&target=" + value).length;
            return (length < threshold);
        });
        return result;
    }

    /*
     * Sometimes we receive a lot of targets. We use the following optimization algorithm:
     *
     * 1. if we are able to draw all targets then draw all of them
     * 2. if we are not able to draw all target then try to find mask and draw targets by mask
     * 3. if we didn't find mask then we truncate targets
     * */

     _optimizeTargets(options, {legendMax = 10, threshold = 3800} = {}) {
         let {target = []} = options;

         if (target.length <= legendMax) {
             return Object.assign(options, {target: target});
         };

         var mask = findMask(target);

         if (null != mask) {
             target = [
                 'alias(lineWidth('+mask+',0.5),"")',
                 'color(lineWidth(averageSeries('+mask+'),5),"ffb03b")'
             ];
             return Object.assign(options, {target: target, hideLegend: "0"});
         }

         return Object.assign(options, {target: target});
    }
}


Graph.propTypes = {
    autoUpdate: React.PropTypes.bool,
    autoUpdateInterval: React.PropTypes.number,
    url: React.PropTypes.string.isRequired,
    graphiteOpts: React.PropTypes.object,
};

Graph.defaultProps = {
    graphiteOpts: {},
    autoUpdate: false,
    autoUpdateInterval: 10000
};

export default Graph;
