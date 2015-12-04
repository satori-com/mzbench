import React from 'react';
import MZBenchActions from '../actions/MZBenchActions';
import MetricsStore from '../stores/MetricsStore';
import Graph from './Graph.react'
import LoadingSpinner from './LoadingSpinner.react';

class BenchGraphs extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            isLoaded: MetricsStore.isDataLoaded(),
            toggles: new Set([0])
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

    renderGraphs(group) {
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
                        <div key={idx} className="col-xs-12 col-md-6">
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
                            {isExpanded ? this.renderGraphs(group) : null}
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

    _onChange() {
        this.setState({isLoaded: MetricsStore.isDataLoaded()});
    }
};

BenchGraphs.propTypes = {
    bench: React.PropTypes.object.isRequired
};

export default BenchGraphs;
