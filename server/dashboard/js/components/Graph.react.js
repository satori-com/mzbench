import moment from 'moment';
import React from 'react';
import MetricsStore from '../stores/MetricsStore';

const MAX_POINTS_PER_GRAPH = 300;
const RUNNING_GRAPH_SHOWED_DURATION = 10; // minutes

class Graph extends React.Component {
    constructor(props) {
        super(props);
        
        this.streams = [];
        this.currentZoom = undefined;
        this.previouslyRunning = undefined;
        this.updatesCounter = 0;
        
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
                            .text(`${this.props.targets[value.line_id - 1]}: ${value.value}`)
                            .classed('mg-area' + value.line_id + '-color', true);
                
                ++lineCount;
            });
        } else {
            const dateString = this._formatDate(data.date);
            rolloverTextContainer.append('tspan').text(dateString).classed(`mg-area${data.line_id}-color`, true);
            
            let label = rolloverTextContainer.append('tspan')
                        .attr({ x: 0, y: 1.1 + 'em' })
                        .text(`${this.props.targets[data.line_id - 1]}: ${data.value}`)
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
                            .text(`${this.props.targets[value.line_id - 1]}: ${value.value}`)
                            .classed('mg-area' + value.line_id + '-color', true);
                
                ++lineCount;
            });
        } else {
            const dateString = this._formatDate(data.date);
            rolloverTextContainer.append('tspan').text(dateString).classed(`mg-area${data.line_id}-color`, true);
            
            let label = rolloverTextContainer.append('tspan')
                        .attr({ x: 0, y: 1.1 + 'em' })
                        .text(`${this.props.targets[data.line_id - 1]}: ${data.value}`)
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
        this._createSubscriptions();
        this._resetState();
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
            graph_options.aggregate_rollover = this.props.targets.length > 1;
        }

        if(this.state.isLoaded) {
            let isEmpty = this.state.data.reduce((prev, dataset) => {
                if (dataset.length > 0) return false;
                else return prev;
            }, true);
            graph_options.data = isEmpty ? [[{date: 0, value: 0, min: 0, max: 0}]] : this.state.data;
            graph_options.legend = this.props.targets;
            
            graph_options.target = document.getElementById(this._graphDOMId());
            
            graph_options.show_confidence_band = ['min', 'max'];
            
            graph_options.xax_format = this._formatDate.bind(this);
            graph_options.mouseover = this._formatRolloverText.bind(this);
            graph_options.x_sort = false;
            
            graph_options.min_x = (this.props.isRunning && !this.props.renderFullscreen)?this.state.maxDate - RUNNING_GRAPH_SHOWED_DURATION*60:0;
            graph_options.max_x = this.state.maxDate;
            
            MG.data_graphic(graph_options);
        } else {
            graph_options.chart_type = 'missing-data';
            graph_options.legend = this.props.targets;
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

    _resetGraphs() {
        this._destroySubscriptions();
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
        this.props.targets.forEach((metric, index) => {
            const streamId = this._subscribeToMetric(this.props.benchId, metric);
            this.streams[index] = streamId;
        });
    }
    
    _destroySubscriptions() {
        this.streams.forEach((streamId) => {
            MetricsStore.unsubscribeFromMetric(streamId);
        });
    }

    _subscribeToMetric(benchId, metric) {
        if(this.props.renderFullscreen) {
            if(this.currentZoom) {
                return MetricsStore.subscribeToMetricSubset(benchId, metric, this._computeSubsamplingInterval(), 
                                                            this.currentZoom.min_x, this.currentZoom.max_x);
            } else {
                return MetricsStore.subscribeToEntireMetric(benchId, metric, this._computeSubsamplingInterval(), false);
            }
        } else {
            if(this.props.isRunning) {
                return MetricsStore.subscribeToMetricWithTimeWindow(benchId, metric, RUNNING_GRAPH_SHOWED_DURATION*60);
            } else {
                return MetricsStore.subscribeToEntireMetric(benchId, metric, this._computeSubsamplingInterval(), false);
            }
        }
    }

    _resetState() {
        this.updatesCounter = 0;
        
        this.setState({
            maxDate: 0, 
            data: this.props.targets.map((metric) => { return []; }), 
            dataBatchs: this.props.targets.map((metric) => { return 0; }), 
            isLoaded: false
        });
    }

    _resolveState() {
        const maxDate = this.props.targets.reduce((maxDate, metric, index) => {
            const metricMaxDate = MetricsStore.getMetricMaxDate(this.streams[index]);
            
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

        const metricData = this.props.targets.map((metric, index) => {
            const data = MetricsStore.getMetricData(this.streams[index]);
            if(data) {
                return data;
            } else {
                return [];
            }
        });

        const metricBatchs = this.props.targets.map((metric, index) => {
            const batchCounter = MetricsStore.getBatchCounter(this.streams[index]);
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
    benchId: React.PropTypes.number.isRequired,
    benchStartTime: React.PropTypes.object,
    benchFinishTime: React.PropTypes.object,
    
    targets: React.PropTypes.array.isRequired,
    isRunning: React.PropTypes.bool,
    title: React.PropTypes.string,
    units: React.PropTypes.string,

    domPrefix: React.PropTypes.string,
    renderFullscreen: React.PropTypes.bool
};

Graph.defaultProps = {
    benchStartTime: undefined,
    benchFinishTime: undefined,
    
    isRunning: false,
    title: "",
    units: "",

    domPrefix: "",
    renderFullscreen: false
};

export default Graph;
