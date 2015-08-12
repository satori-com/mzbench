import React, { PropTypes } from 'react';
import Graph from './Graph.react'
import LoadingSpinner from './LoadingSpinner.react';

class BenchGraphs extends React.Component {
    constructor(props) {
        super(props);
        this.state = {toggles: new Set([0])};
    }

    _onToggle(idx) {
        let { toggles } = this.state;
        toggles.has(idx) ? toggles.delete(idx) : toggles.add(idx);
        this.setState({toggles: toggles});
    }

    renderWaitMetrics() {
        return (
            <LoadingSpinner>Waiting metrics from node...</LoadingSpinner>
        );
    }

    renderGraphGroup(group) {
        return (
            <div className="panel-body">
                {group.graphs.map((graph, idx) => {
                    return (
                        <div key={idx} className="col-xs-6 col-md-6">
                            <Graph
                                url={this.props.bench.graphite + "/render/"}
                                graphiteOpts={{
                                    target: graph.metrics,
                                    title: graph.name,
                                    from: "09:21_20150729",
                                    until: "09:39_20150729",
                                    width: "555",
                                    height: "418"
                                }} />
                        </div>
                    );
                })}
            </div>
        );
    }


    renderGraphs() {
        return (
            <div>
                {this.props.bench.demoGroups.map((group, idx) => {
                    let isExpanded = this.state.toggles.has(idx);
                    let iconClass = isExpanded ? "glyphicon-collapse-down" : "glyphicon-expand";
                    return (
                        <div key={idx} className="panel panel-default graph-panel">
                            <div className="panel-heading">
                                <h3 className="panel-title"> 
                                    <span className={`glyphicon ${iconClass}`} />&nbsp;
                                    <span className="graph-group-title" role="button" href="#" onClick={this._onToggle.bind(this, idx)}>{group.name}</span>
                                </h3>
                            </div>
                            {isExpanded ? this.renderGraphGroup(group) : null}
                        </div>);
                })}
            </div>
        );
    }

    render() {
        if (!this.props.bench.demoGroups) {
            return this.renderWaitMetrics();
        }

        return this.renderGraphs();
    }
}

BenchGraphs.propTypes = {
    bench: React.PropTypes.object.isRequired
}

export default BenchGraphs;
