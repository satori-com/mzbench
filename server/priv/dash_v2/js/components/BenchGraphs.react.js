import React, { PropTypes } from 'react';
import Graph from './Graph.react'
import LoadingSpinner from './LoadingSpinner.react';

class BenchGraphs extends React.Component {
    constructor(props) {
        super(props);
        this.state = {toggles: new Set([0])};
    }

    renderWaitMetrics() {
        return (
            <LoadingSpinner>Waiting metrics from node...</LoadingSpinner>
        );
    }

    renderUnknownGraphite() {
        return (
            <div className="alert alert-warning" role="alert">
                <strong>Graphite is not specified!</strong> You should specify the Graphite server in the server config. You could find addition details by the following <a href="https://github.com/machinezone/mzbench/blob/master/doc/deployment_guide.md#configuration-file-format" target="_blank">link</a>.
            </div>
        );
    }

    renderEmptyGraphs() {
        return (
            <div className="panel-body">
                <div className="alert alert-warning" role="alert">
                    <strong>Oh snap!</strong> This group doesn't have any graphs
                </div>
            </div>
        );
    }

    renderEmptyGroups() {
        return (
            <div className="alert alert-warning" role="alert">
                <strong>Oh snap!</strong> This bench hasn't recordered any metrics. See <a href="#">Logs</a> for the additional information.
            </div>
        );
    }

    renderGraphGroup(group) {
        const graphitePrefix = this.props.bench.metrics.graphite_prefix;
        const graphiteUrl = this.props.bench.metrics.graphite_url;
        const graphs = group.graphs || [];

        if (0 == graphs.length) {
            return this.renderEmptyGroups();
        }

        return (
            <div className="panel-body">
                {graphs.map((graph, idx) => {

                    let targets = graph.metrics.map((m) => {
                        const parts = [graphitePrefix, m.name];
                        return parts.filter(x => x).join(".");
                    });

                    return (
                        <div key={idx} className="col-xs-6 col-md-6">
                            <Graph
                                url={graphiteUrl + "/render/"}
                                bench={this.props.bench}
                                graphiteOpts={{
                                    target: targets,
                                    title: graph.title,
                                    vtitle: graph.units,
                                    width: 555,
                                    height: 418
                                }} />
                        </div>
                    );
                })}
            </div>
        );
    }

    renderGraphs() {
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
                            {isExpanded ? this.renderGraphGroup(group) : null}
                        </div>);
                })}
            </div>
        );
    }

    render() {
        const bench = this.props.bench;
        const hasGraphite = bench.metrics.graphite_url;
        const hasGroups = bench.metrics.groups;

        if (bench.isRunning() && !hasGroups) {
            return this.renderWaitMetrics();
        }

        if (hasGroups && !hasGraphite) {
            return this.renderUnknownGraphite();
        }

        return this.renderGraphs();
    }

    _onToggle(idx) {
        let { toggles } = this.state;
        toggles.has(idx) ? toggles.delete(idx) : toggles.add(idx);
        this.setState({toggles: toggles});
    }
}

BenchGraphs.propTypes = {
    bench: React.PropTypes.object.isRequired
}

export default BenchGraphs;
