import moment from 'moment';
import React from 'react';
import MetricsStore from '../stores/MetricsStore';

const RUNNING_GRAPH_SHOWED_DURATION = 10; // minutes

class Graph extends React.Component {
    constructor(props) {
        super(props);
        this.previously_running = undefined;
        this.targets_max_date = [];
        this.state = this._resolveState();
        this._onChange = this._onChange.bind(this);
    }
    
    componentDidMount() {
        MetricsStore.onChange(this._onChange);
        this.previously_running = this.props.is_running;
        this.targets_max_date = this.state.data.map((dataset) => { return dataset.length; });
        MetricsStore.addSubscription(this.props.targets);
        this._renderGraph();
    }
    
    componentWillUnmount() {
        MetricsStore.off(this._onChange);
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
            setTimeout(this._updateGraph.bind(this), 1);
            return false;
        }
    }
    
    componentDidUpdate() {
        this._onChange();
        setTimeout(this._renderGraph.bind(this), 1);
    }
    
    render() {
        return (
            <div>
                <div id={this._graphDOMId()}></div>
                <div id={this._legendDOMId()} className="graph-legend"></div>
            </div>
        );
    }
    
    _graphDOMId() {
        return this.props.dom_prefix + this.props.targets[0].replace(/\./g, "-") + "-graph";
    }
    
    _legendDOMId() {
        return this.props.dom_prefix + this.props.targets[0].replace(/\./g, "-") + "-legend";
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
    
    _formatRolloverText(data, i) {
        let rollover_text_container = d3.select('#' + this._graphDOMId() + ' svg .mg-active-datapoint');
        rollover_text_container.selectAll('*').remove();
        
        if(data.key) {
            const date_string = this._formatDate(data.values[0].date);
            rollover_text_container.append('tspan').text(date_string);
            
            let lineCount = 1;
            let lineHeight = 1.1;
            data.values.forEach((value) => {
                let label = rollover_text_container.append('tspan')
                            .attr({ x: 0, y: (lineCount * lineHeight) + 'em' })
                            .text(value.value);
                
                rollover_text_container.append('tspan')
                .attr({ x: -label.node().getComputedTextLength(), y: (lineCount * lineHeight) + 'em' })
                .text('\u2014 ')
                .classed('mg-line' + value.line_id + '-color', true);
                
                ++lineCount;
            });
        } else {
            const date_string = this._formatDate(data.date);
            const text = `${date_string} -> ${data.value}`;
            
            rollover_text_container.append('tspan').text(text);
        }
    }

    _renderGraph() {
        let graph_options = {
            title: this.props.title,
            y_label: this.props.units,
            missing_text: "No data was reported yet for these metrics.",

            buffer: 0,
            left: 65,
            right: 0,
            height: 400,

            area: false,
            brushing: false,
            interpolate: 'linear',
            x_extended_ticks: true,
            y_extended_ticks: true,
            full_width: true,
            aggregate_rollover: true,
            transition_on_update: false
        };
        
        if(this.props.render_fullscreen) {
            graph_options.width = window.innerWidth - 100;
            graph_options.height = window.innerHeight - 200;
            graph_options.full_width = false;
            
            graph_options.brushing = true;
        }
        
        let data_is_ready = this.state.data.reduce((result, data) => {
            if (data.length > 0) {
                return result;
            } else {
                return false;
            }
        }, true);

        
        if(data_is_ready) {
            graph_options.data = this.state.data;
            graph_options.legend = this.props.targets;
            
            graph_options.target = document.getElementById(this._graphDOMId());
            graph_options.legend_target = document.getElementById(this._legendDOMId());
            
            graph_options.x_accessor = 'date';
            graph_options.xax_format = this._formatDate.bind(this);
            graph_options.y_accessor = 'value';
            graph_options.mouseover = this._formatRolloverText.bind(this);
            graph_options.x_sort = false;
            
            graph_options.brushing = !this.props.is_running;
            
            graph_options.min_x = this.props.is_running?this.state.max_date - RUNNING_GRAPH_SHOWED_DURATION*60:0;
            graph_options.max_x = this.state.max_date;
            
            MG.data_graphic(graph_options);
        } else {
            graph_options.chart_type = 'missing-data';
            graph_options.legend = this.props.targets;
            graph_options.target = document.getElementById(this._graphDOMId());
            graph_options.legend_target = document.getElementById(this._legendDOMId());
            
            MG.data_graphic(graph_options);
        }
    }
    
    _updateGraph() {
        if(this.previously_running != this.props.is_running) {
            this._renderGraph();
            this.previously_running = this.props.is_running;
        } else {
            const data_updated = this.targets_max_date.reduce((result, prev_max_data, target_idx) => {
                if(target_idx < this.state.data.length) {
                    if(prev_max_data != this.state.data[target_idx].length) {
                        return true;
                    } else {
                        return result;
                    }
                } else {
                    return true;
                }
            }, false);

            if(data_updated) {
                this._renderGraph();
                this.targets_max_date = this.state.data.map((dataset) => { return dataset.length; });
            }
        }
    }
    
    _resolveState() {
        const max_date = this.props.targets.reduce((max_date, metric) => {
            const metric_max_date = MetricsStore.getMetricMaxDate(metric);
            if(metric_max_date > max_date) {
                return metric_max_date;
            } else {
                return max_date;
            }
        }, 0);

        const metricData = this.props.targets.map((metric) => {
                return MetricsStore.getMetricData(metric);
            })
        return {
            max_date: max_date,
            data: metricData
        };
    }

    _onChange() {
        this.setState(this._resolveState());
    }
};

Graph.propTypes = {
    targets: React.PropTypes.array.isRequired,
    is_running: React.PropTypes.bool,
    title: React.PropTypes.string,
    units: React.PropTypes.string,
    
    dom_prefix: React.PropTypes.string,
    render_fullscreen: React.PropTypes.bool
};

Graph.defaultProps = {
    is_running: false,
    title: "",
    units: "",
    
    dom_prefix: "",
    render_fullscreen: false
};

export default Graph;
