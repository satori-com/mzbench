import React from 'react';
import MZBenchActions from '../actions/MZBenchActions';
import MetricsStore from '../stores/MetricsStore';
import Graph from './Graph.react';
import Modal from './Modal.react';
import LoadingSpinner from './LoadingSpinner.react';

class BenchGraphs extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            isLoaded: MetricsStore.isDataLoaded(),
            toggles: new Set([0]),
            fullScreenGraph: undefined
        };

        var benchId = this.props.bench.id;
        MetricsStore.resetSubscriptions(benchId);

        this._onChange = this._onChange.bind(this);
    }

    componentDidMount() {
        MetricsStore.onChange(this._onChange);
    }

    componentWillUnmount() {
        MetricsStore.off(this._onChange);
    }

    renderWaitMetrics() {
        return (
            <LoadingSpinner>Waiting metrics from node...</LoadingSpinner>
        );
    }

    renderEmptyGroups() {
        const link = `#/bench/${this.props.bench.id}/logs`;
        return (
            <div className="alert alert-warning" role="alert">
                <strong>Oh snap!</strong> This bench has not recordered any metrics. See <a href={link}>Logs</a> for the additional information.
            </div>
        );
    }

    renderModalGraph() {
        let fullScreenGraph = this.state.fullScreenGraph;
        if(fullScreenGraph) {
            const group = this.props.bench.metrics.groups[fullScreenGraph.group_idx];
            const graph = group.graphs[fullScreenGraph.graph_idx];
            const targets = graph.metrics.map((m) => {
                return m.name;
            });

            return (
                <Graph is_running={this.props.bench.isRunning()} targets={targets}
                    title={graph.title} units={graph.units} bench_id={this.props.bench.id}
                    dom_prefix="modal-" render_fullscreen={true}/>
            );
        } else {
            return;
        }
    }

    renderGraphs(group_idx, group) {
        const graphs = group.graphs || [];

        if (0 == graphs.length) {
            return this.renderEmptyGroups();
        }

        return (
            <div className="panel-body">
                {graphs.map((graph, idx) => {

                    let targets = graph.metrics.map((m) => {
                        return m.name;
                    });

                    return (
                        <div key={idx} className="col-xs-12 col-md-6" onClick={this._onGraphClick.bind(this, group_idx, idx)}>
                            <Graph is_running={this.props.bench.isRunning()} targets={targets}
                                title={graph.title} units={graph.units} bench_id={this.props.bench.id}/>
                        </div>
                    );
                })}
            </div>
        );
    }

    renderGroups() {
        const groups = this.props.bench.metrics.groups || [];

        if (0 == groups.length) {
            return this.renderEmptyGroups();
        }

        return (
            <div>
                <Modal ref="fullScreenGraphModal" onOk={this._onCloseGraph} render_fullscreen={true} 
                        render_title={false} render_submit_button={false}>
                    {this.renderModalGraph()}
                </Modal>
            
                {groups.map((group, idx) => {
                    let isExpanded = this.state.toggles.has(idx);
                    let iconClass = isExpanded ? "glyphicon-collapse-down" : "glyphicon-expand";
                    return (
                        <div key={idx} className="panel panel-default graph-panel">
                            <div className="panel-heading">
                                <h3 className="panel-title">
                                    <span className={`glyphicon ${iconClass}`} />&nbsp;
                                    <span className="graph-group-title" role="button" onClick={this._onToggle.bind(this, idx)}>{group.name}</span>
                                </h3>
                            </div>
                            {isExpanded ? this.renderGraphs(idx, group) : null}
                        </div>);
                })}
            </div>
        );
    }

    render() {
        const bench = this.props.bench;
        const hasGroups = bench.metrics.groups;

        if ((bench.isRunning() && !hasGroups) || !this.state.isLoaded) {
            return this.renderWaitMetrics();
        }

        return this.renderGroups();
    }

    _onToggle(idx) {
        let { toggles } = this.state;
        toggles.has(idx) ? toggles.delete(idx) : toggles.add(idx);
        this.setState({toggles: toggles});
    }
    
    _onGraphClick(group_idx, graph_idx) {
        this.setState({fullScreenGraph: {group_idx: group_idx, graph_idx: graph_idx}});
        this.refs.fullScreenGraphModal.open();
    }
    
    _onCloseGraph() {
        this.refs.fullScreenGraphModal.close();
    }

    _onChange() {
        this.setState({isLoaded: MetricsStore.isDataLoaded()});
    }
};

BenchGraphs.propTypes = {
    bench: React.PropTypes.object.isRequired
};

export default BenchGraphs;
