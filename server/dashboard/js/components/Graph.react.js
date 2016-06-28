import moment from 'moment';
import React from 'react';
import MetricsStore from '../stores/MetricsStore';

const MAX_POINTS_PER_GRAPH = 300;
const RUNNING_GRAPH_SHOWED_DURATION = 10; // minutes
const MIN_GRAPHS_TO_BEGIN_COMPRESSION = 20;

class _DataStream {
    constructor(name, aggregated, metrics, benchId, kind, benchIds, x_env) {
        this.name = name;
        this.aggregated = aggregated;
        this.metrics = metrics;
        this.benchId = benchId;
        this.streams = [];
        this.kind = kind;
        this.benchIds = benchIds;
        this.x_env = x_env;
    }
    
    subscribeToEntireMetric(subsamplingInterval, continueStreamingAfterEnd) {
        if (this.kind == "ordinary" || this.kind == "compare") {
            this.streams = this.metrics.map((metric) => {
                return MetricsStore.subscribeToEntireMetric(this.benchId, metric, subsamplingInterval, continueStreamingAfterEnd, this.kind == "compare");
            });
            this._createAggregatedMetric(this.benchId, undefined);
        } else {
            this.streams = this.metrics.map((metric) => {
               return MetricsStore.subscribeToFinalResults(metric, this.benchIds, this.kind, this.x_env);
            });
        }
    }
    
    subscribeToMetricSubset(subsamplingInterval, beginTime, endTime) {
        if (this.kind != "ordinary") throw "Subscribe to a subset is not supported for cummulative metrics";

        this.streams = this.metrics.map((metric) => {
            return MetricsStore.subscribeToMetricSubset(this.benchId, metric, subsamplingInterval, beginTime, endTime);
        });
        this._createAggregatedMetric(this.benchId, undefined);
    }
    
    subscribeToMetricWithTimeWindow(timeInterval) {
        if (this.kind != "ordinary") throw "Subscribe with time window is not supported for cummulative metrics";

        this.streams = this.metrics.map((metric) => {
            return MetricsStore.subscribeToMetricWithTimeWindow(this.benchId, metric, timeInterval);
        });
        this._createAggregatedMetric(this.benchId, timeInterval);
    }
    
    unsubscribeFromMetric() {
        this._destroyAggregatedMetric();
        this.streams.forEach((streamId) => {
            MetricsStore.unsubscribeFromMetric(streamId);
        });
    }
    
    getMetricMaxDate() {
        return MetricsStore.getMetricMaxDate(this.streams[this.streams.length - 1]);
    }
    
    getMetricData() {
        return MetricsStore.getMetricData(this.streams[this.streams.length - 1]);
    }
    
    getBatchCounter() {
        return MetricsStore.getBatchCounter(this.streams[this.streams.length - 1]);
    }
    
    _createAggregatedMetric(timeInterval) {
        if(this.aggregated) {
            const id = MetricsStore.createAggregatedStream(this.benchId, this.streams.slice(), timeInterval);
            this.streams.push(id);
        }
    }
    
    _destroyAggregatedMetric() {
        if(this.aggregated) {
            const id = this.streams.pop();
            MetricsStore.removeAggregatedStream(id);
        }
    }
};

class Graph extends React.Component {
    constructor(props) {
        super(props);
        
        this.streams = [];
        this.currentZoom = undefined;
        this.previouslyRunning = undefined;
        this.updatesCounter = 0;
        
        this._createStreams();
        
        this.state = this._resolveState();
        this._onChange = this._onChange.bind(this);
        this._updateGraph = this._updateGraph.bind(this);
    }
    
    componentDidMount() {
        MetricsStore.onChange(this._onChange);
        this.previouslyRunning = this.props.isRunning;
        
        this._createSubscriptions();
        this._renderGraph();
    }
    
    componentWillUnmount() {
        MetricsStore.off(this._onChange);
        
        this._destroySubscriptions();
    }
    
    shouldComponentUpdate(nextProps, nextState) {
        if(nextProps.targets[0] != this.props.targets[0]) {
            // The change of the first target will change the DOM Ids
            // of the MetricsGraphics.js targets. Its the only case
            // when rendering should be done.
            //
            // Happens in fullscreen mode.
            return true;
        } else {
            // Graph should probably be updated, but not the DOM.
            setTimeout(this._updateGraph, 1);
            return false;
        }
    }
    
    componentDidUpdate() {
        this._onChange();
        setTimeout(this._renderGraph.bind(this), 1 + Math.round(3000*Math.random()));
    }
    
    render() {
        return (<div id={this._graphDOMId()}></div>);
    }
    
    _graphDOMId() {
        return this.props.domPrefix + this.props.targets[0].replace(/\./g, "-") + "-graph";
    }
    
    _formatClockNumber(number) {
        let result = number.toString();
        if(number < 10) {
            result = "0" + result;
        }
        return result;
    }
    
    _formatDate(rawDate) {
        if (this.props.kind == "group")
            return "" + rawDate;

        const absDate = (rawDate < 0)?-1*rawDate:rawDate;
        const negDate = rawDate < 0;
        let date = moment.duration(absDate, 'seconds');
        
        let result = `${this._formatClockNumber(date.minutes())}:${this._formatClockNumber(date.seconds())}`;
        if(date.hours() > 0) {
            result = `${this._formatClockNumber(date.hours())}:` + result;
        }
        if(date.days() > 0) {
            result = `${date.days()}d ` + result;
        }
        if(date.months() > 0) {
            result = `${date.months()}m ` + result;
        }
        if(date.years() > 0) {
            result = `${date.years()}y ` + result;
        }
        
        if(negDate) {
            return '-' + result;
        } else {
            return result;
        }
    }
    
    _formatRolloverTextFullscreen(data, i) {
        let rolloverTextContainer = d3.select('#' + this._graphDOMId() + ' svg .mg-active-datapoint');
        rolloverTextContainer.selectAll('*').remove();
        
        if(data.key) {
            const dateString = this._formatDate(data.values[0].date);
            rolloverTextContainer.append('tspan').text(dateString).classed('mg-area1-color', true);
            
            let lineCount = 1;
            let lineHeight = 1.1;
            data.values.forEach((value) => {
                let label = rolloverTextContainer.append('tspan')
                            .attr({ x: 0, y: (lineCount * lineHeight) + 'em' })
                            .text(`${this.streams[value.line_id - 1].name}: ${value.value}`)
                            .classed('mg-area' + value.line_id + '-color', true);
                
                ++lineCount;
            });
        } else {
            const dateString = this._formatDate(data.date);
            rolloverTextContainer.append('tspan').text(dateString).classed(`mg-area${data.line_id}-color`, true);
            
            let label = rolloverTextContainer.append('tspan')
                        .attr({ x: 0, y: 1.1 + 'em' })
                        .text(`${this.streams[data.line_id - 1].name}: ${data.value}`)
                        .classed(`mg-area${data.line_id}-color`, true);
        }
    }
    
    _formatRolloverTextNormal(data, i) {
        let rolloverTextContainer = d3.select('#' + this._graphDOMId() + ' svg .mg-active-datapoint');
        rolloverTextContainer.selectAll('*').remove();
        
        if(data.key) {
            const dateString = this._formatDate(data.values[0].date);
            rolloverTextContainer.append('tspan').text(dateString).classed('mg-area1-color', true);
            
            let lineCount = 1;
            let lineHeight = 1.1;
            data.values.forEach((value) => {
                let label = rolloverTextContainer.append('tspan')
                            .attr({ x: 0, y: (lineCount * lineHeight) + 'em' })
                            .text(`${this.streams[value.line_id - 1].name}: ${value.value}`)
                            .classed('mg-area' + value.line_id + '-color', true);
                
                ++lineCount;
            });
        } else {
            const dateString = this._formatDate(data.date);
            rolloverTextContainer.append('tspan').text(dateString).classed(`mg-area${data.line_id}-color`, true);
            
            let label = rolloverTextContainer.append('tspan')
                        .attr({ x: 0, y: 1.1 + 'em' })
                        .text(`${this.streams[data.line_id - 1].name}: ${data.value}`)
                        .classed(`mg-area${data.line_id}-color`, true);
        }
    }
    
    _formatRolloverText(data, i) {
        if(this.props.renderFullscreen) {
            this._formatRolloverTextFullscreen(data, i);
        } else {
            this._formatRolloverTextNormal(data, i);
        }
    }

    _performZoom(step) {
        this.currentZoom = step;
        this._destroySubscriptions();
        this._createStreams();
        this._resetState();
        this._createSubscriptions();
    }

    _calcDataMin() {
        return this.state.data.reduce((acc, stream) => {
            const stream_min = stream.reduce((acc, value) => {
                if(acc === undefined || value.min < acc) return value.min;
                else return acc;
            }, undefined);
            
            if(acc === undefined || stream_min < acc) return stream_min;
            else return acc;
        }, undefined);
    }

    _calcDataMax() {
        return this.state.data.reduce((acc, stream) => {
            const stream_max = stream.reduce((acc, value) => {
                if(acc === undefined || value.max > acc) return value.max;
                else return acc;
            }, undefined);
            
            if(acc === undefined || stream_max > acc) return stream_max;
            else return acc;
        }, undefined);
    }

    _renderGraph() {
        let graph_options = {
            title: this.props.title,
            y_label: this.props.units,
            missing_text: "Loading...",

            buffer: 0,
            left: 65,
            right: 0,
            height: 400,

            area: false,
            brushing: false,
            brushing_history: false,
            interpolate: 'monotone',
            x_extended_ticks: true,
            y_extended_ticks: true,
            full_width: true,
            aggregate_rollover: false,
            transition_on_update: false
        };
        
        if(this.props.renderFullscreen) {
            graph_options.width = window.innerWidth - 100;
            graph_options.height = window.innerHeight - 200;
            graph_options.full_width = false;
            
            graph_options.brushing = true;
            graph_options.brushing_history = true;
            graph_options.after_brushing = this._performZoom.bind(this);
            graph_options.brushing_manual_redraw = true;
            graph_options.aggregate_rollover = this.streams.length > 1;
        }

        if(this.state.isLoaded) {
            let isEmpty = this.state.data.reduce((prev, dataset) => {
                if (dataset.length > 0) return false;
                else return prev;
            }, true);
            graph_options.data = isEmpty ? [[{date: 0, value: 0, min: 0, max: 0}]] : this.state.data;
            graph_options.legend = this.streams.map((stream) => { return stream.name; });
            
            graph_options.target = document.getElementById(this._graphDOMId());
            
            graph_options.show_confidence_band = ['min', 'max'];
            
            graph_options.xax_format = this._formatDate.bind(this);
            graph_options.mouseover = this._formatRolloverText.bind(this);
            graph_options.x_sort = false;
            
            graph_options.min_x = (this.props.isRunning && !this.props.renderFullscreen)?this.state.maxDate - RUNNING_GRAPH_SHOWED_DURATION*60:0;
            graph_options.max_x = this.state.maxDate;
            
            const data_min = this._calcDataMin();
            graph_options.min_y = (data_min < 0)?data_min:0;
            graph_options.max_y = this._calcDataMax();
            
            MG.data_graphic(graph_options);
        } else {
            graph_options.chart_type = 'missing-data';
            graph_options.legend = this.streams.map((stream) => { return stream.name; });
            graph_options.target = document.getElementById(this._graphDOMId());
            
            MG.data_graphic(graph_options);
        }
    }

    _updateGraph() {
        if (!this.state.isLoaded) {
            return;
        }

        if (this.previouslyRunning != this.props.isRunning) {
            this.previouslyRunning = this.props.isRunning;
            this._resetGraphs();
        } else {
            const newUpdatesCounter = this.state.dataBatchs.reduce((a, b) => { return a + b }, 0);
            let dataUpdated = newUpdatesCounter > this.updatesCounter;

            if(dataUpdated) {
                this._renderGraph();
                this.updatesCounter = newUpdatesCounter;
            }
        }
    }

    _createStreams() {
        if(this.props.renderFullscreen
                || this.props.targets.length < MIN_GRAPHS_TO_BEGIN_COMPRESSION) {
            if (this.props.kind == "") {
                this.streams = this.props.targets.map((metric) => {
                    let m = [];
                    m.push(metric);
                    return new _DataStream(metric, false, m, this.props.benchId, "ordinary", [], "");
                });
            } else {
                this.props.targets.forEach((metric) => {
                    let m = [];
                    m.push(metric);
                    if (this.props.kind == "compare") {
                        this.props.benchset.forEach((B) => {
                            let Id = B.benches[0].id;
                            this.streams.push(new _DataStream(B.name+":"+Id, false, m, Id, this.props.kind, [], ""));
                        });
                    } else if (this.props.kind == "regression") {
                        let Ids = this.props.benchset.map((B) => B.benches[0].id);
                        this.streams.push(new _DataStream(metric, false, m, undefined, this.props.kind, Ids, ""));
                    } else if (this.props.kind == "group") {
                        this.props.benchset.forEach((B) => {
                            let Ids = B.benches.map((bench) => bench.id);
                            this.streams.push(new _DataStream(B.name, false, m, undefined, this.props.kind, Ids, this.props.x_env));
                        });
                    }
                });
            }

        } else {
            this.streams = [];
        
            let metricsToAggregate = [];
            for(let i = 0; i < this.props.targets.length; i++) {
                if(this.props.targets[i].match("systemload.*mzb_worker") === null) {
                    let m = [];
                    m.push(this.props.targets[i]);
                    this.streams.push(new _DataStream(this.props.targets[i], false, m, this.props.benchId, "ordinary", [], ""));
                } else {
                    metricsToAggregate.push(this.props.targets[i]);
                }
            }
            
            if(metricsToAggregate.length !== 0) {
                let tmp = metricsToAggregate[0].split(".");
                tmp.pop();
                const metricName = tmp.join(".") + ".mzb_worker.aggregated";
                this.streams.push(new _DataStream(metricName, true, metricsToAggregate, this.props.benchId, "ordinary", [], ""));
            }
        }
    }

    _resetGraphs() {
        this._destroySubscriptions();
        this._createStreams();
        this._resetState();
        this._createSubscriptions();
    }

    _computeSubsamplingInterval() {
        if(this.currentZoom) {
            const duration = this.currentZoom.max_x - this.currentZoom.min_x;
            return Math.floor(duration/MAX_POINTS_PER_GRAPH);
        }
        
        if(this.props.benchStartTime) {
            const lastActiveTime = this.props.isRunning ? moment() : this.props.benchFinishTime;
            const benchDuration = lastActiveTime.diff(this.props.benchStartTime, 'seconds');
            
            return Math.floor(benchDuration/MAX_POINTS_PER_GRAPH);
        } else {
            return 0;
        }
    }

    _createSubscriptions() {
        this.streams.forEach((stream) => {
            this._subscribeToMetric(stream);
        });
    }
    
    _destroySubscriptions() {
        this.streams.forEach((stream) => {
            stream.unsubscribeFromMetric();
        });
    }

    _subscribeToMetric(stream) {
        if(this.props.renderFullscreen) {
            if(this.currentZoom) {
                stream.subscribeToMetricSubset(this._computeSubsamplingInterval(),
                    this.currentZoom.min_x, this.currentZoom.max_x);
            } else {
                stream.subscribeToEntireMetric(this._computeSubsamplingInterval(), false);
            }
        } else {
            if(this.props.isRunning) {
                stream.subscribeToMetricWithTimeWindow(RUNNING_GRAPH_SHOWED_DURATION*60);
            } else {
                stream.subscribeToEntireMetric(this._computeSubsamplingInterval(), false);
            }
        }
    }

    _resetState() {
        this.updatesCounter = 0;
        
        this.setState({
            maxDate: 0, 
            data: this.streams.map((stream) => { return []; }), 
            dataBatchs: this.streams.map((streams) => { return 0; }), 
            isLoaded: false
        });
    }

    _resolveState() {
        const maxDate = this.streams.reduce((maxDate, stream) => {
            const metricMaxDate = stream.getMetricMaxDate();
            
            if(metricMaxDate) {
                if(metricMaxDate > maxDate) {
                    return metricMaxDate;
                } else {
                    return maxDate;
                }
            } else {
                return maxDate;
            }
        }, 0);

        const metricData = this.streams.map((stream) => {
            const data = stream.getMetricData();
            if(data) {
                return data;
            } else {
                return [];
            }
        });

        const metricBatchs = this.streams.map((stream) => {
            const batchCounter = stream.getBatchCounter();
            if(batchCounter) {
                return batchCounter;
            } else {
                return 0;
            }
        });

        const isLoaded = metricBatchs.reduce((prev, v) => {
            if (v <= 0) return false;
            else return prev;
        }, true);

        return {
            maxDate: maxDate,
            data: metricData,
            dataBatchs: metricBatchs,
            isLoaded: isLoaded
        };
    }

    _onChange() {
        this.setState(this._resolveState());
    }
};

Graph.propTypes = {
    benchId: React.PropTypes.number,
    benchStartTime: React.PropTypes.object,
    benchFinishTime: React.PropTypes.object,
    
    targets: React.PropTypes.array.isRequired,
    isRunning: React.PropTypes.bool,
    title: React.PropTypes.string,
    units: React.PropTypes.string,

    kind: React.PropTypes.string,
    x_env: React.PropTypes.string,
    benchset: React.PropTypes.array,

    domPrefix: React.PropTypes.string,
    renderFullscreen: React.PropTypes.bool
};

Graph.defaultProps = {
    benchId: undefined,
    benchStartTime: undefined,
    benchFinishTime: undefined,
    
    isRunning: false,
    title: "",
    units: "",

    kind: "", // For cummulative benches
    x_env: "",
    benchset: [], // Empty when a single bench is shown

    domPrefix: "",
    renderFullscreen: false
};

export default Graph;
