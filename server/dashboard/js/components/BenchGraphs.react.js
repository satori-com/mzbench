import React from 'react';
import MZBenchActions from '../actions/MZBenchActions';
import Graph from './Graph.react';
import GraphModal from './GraphModal.react';
import LoadingSpinner from './LoadingSpinner.react';
import MZBenchRouter from '../utils/MZBenchRouter';
import BenchStore from '../stores/BenchStore';

class BenchGraphs extends React.Component {
    constructor(props) {
        super(props);

        this.state = { toggles: BenchStore.getToggledSet(this.props.bench.id) };
        this.isGraphOpen = false;
    }

    componentDidMount() {
        if (this.props.activeGraph && !this.isGraphOpen) {
            this.refs.fullScreenGraphModal.open();
            this.isGraphOpen = true
        }
    }

    componentDidUpdate() {
        if (this.props.activeGraph && !this.isGraphOpen) {
            this.refs.fullScreenGraphModal.open();
            this.isGraphOpen = true
        }
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
        let fullScreenGraph = this.props.activeGraph;
        if(fullScreenGraph) {
            const group = this.props.bench.metrics.groups[fullScreenGraph.groupId];
            const graph = group.graphs[fullScreenGraph.graphId];
            const targets = graph.metrics.map((m) => {
                return m.name;
            });

            return (
                <Graph isRunning={this.props.bench.isRunning()} targets={targets} kind={graph.metatype}
                    title={graph.title} units={graph.units} benchId={this.props.bench.id}
                    benchStartTime={this.props.bench.start_time_client} benchFinishTime={this.props.bench.finish_time_client}
                    domPrefix="modal-" renderFullscreen={true}/>
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

                    let targets = graph.metrics.reduce((acc, m) => {
                        if (m.visibility) {
                            acc.push(m.name);
                        }
                        return acc;
                    }, []);

                    if (0 == targets.length) {
                        return;
                    }

                    const link = `#/bench/${this.props.bench.id}/graphs/${group_idx}/${idx}`;

                    return (
                        <div key={""+idx+"("+targets.length+")"} className="col-xs-12 col-md-6">
                            <a href={link} className="bs-link">
                                <Graph isRunning={this.props.bench.isRunning()} targets={targets} kind={graph.metatype}
                                    title={graph.title} units={graph.units} benchId={this.props.bench.id}
                                    benchStartTime={this.props.bench.start_time_client} benchFinishTime={this.props.bench.finish_time_client}
                                    />
                            </a>
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
                <GraphModal ref="fullScreenGraphModal" onOk={this._onCloseGraph.bind(this)}>
                    {this.renderModalGraph()}
                </GraphModal>

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

        if ((bench.isRunning() && !hasGroups)) {
            return this.renderWaitMetrics();
        }

        return this.renderGroups();
    }

    _onToggle(idx) {
        let { toggles } = this.state;
        toggles.has(idx) ? toggles.delete(idx) : toggles.add(idx);
        MZBenchActions.saveToggledGraphs(this.props.bench.id, toggles);
        this.setState({toggles: toggles});
    }

    _onCloseGraph() {
        MZBenchRouter.navigate(`/bench/${this.props.bench.id}/overview`, {});
        MZBenchActions.deselectGraph();
        this.refs.fullScreenGraphModal.close();
        this.isGraphOpen = false;
    }
};

BenchGraphs.propTypes = {
    bench: React.PropTypes.object.isRequired
};

export default BenchGraphs;
