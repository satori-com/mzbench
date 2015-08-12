import React, { PropTypes } from 'react';

import MetricStore from '../stores/MetricStore';
import LoadingSpinner from './LoadingSpinner.react';

class BenchMetrics extends React.Component {
    constructor(props) {
        super(props);
        this.state = { snapshot: this._getSnapshot(props.bench.metrics) };
        this._onChange = this._onChange.bind(this);
    }

    componentDidMount() {
        MetricStore.onChange(this._onChange);
    }

    componentWillUnmount() {
        MetricStore.off(this._onChange);
    }

    componentWillReceiveProps(props) {
        this.setState({ snapshot: this._getSnapshot(props.bench.metrics) });
    }

    renderDownloadReportPanel() {
        return (
            <div className="panel panel-default">
                <div className="panel-heading">
                    <h3 className="panel-title">Download report&nbsp;
                    {this.props.bench.isRunning() ? <small>(enabled for finished benches only)</small> : null}
                    </h3>
                </div>
                <div className="panel-body">
                    <button className="btn btn-primary" type="submit" disabled={this.props.bench.isRunning()}>Text</button>&nbsp;
                    <button className="btn btn-primary" type="submit" disabled={this.props.bench.isRunning()}>CSV</button>&nbsp;
                    <button className="btn btn-primary" type="submit" disabled={this.props.bench.isRunning()}>JSON</button>
                </div>
            </div>
        );
    }

    renderRealtimeMetrics() {
        return (
            <table className="table table-striped">
                <thead>
                  <tr>
                    <th>Name</th>
                    <th>Value</th>
                  </tr>
                </thead>

                <tbody>
                    {this.state.snapshot.map(({name,value}) => {
                        return (
                            <tr key={name}>
                                <td>{name}</td>
                                <td>{value}</td>
                            </tr>
                        );
                    })}
                </tbody>
            </table>
        );
    }

    renderStatMetrics() {
        let randInt = (n) => Math.floor((Math.random() * n));
        let flatten = this._flatten(this.props.bench.metrics);
        return (
            <table className="table table-striped">
                <thead>
                  <tr>
                    <th>Name</th>
                    <th>min</th>
                    <th>avg</th>
                    <th>max</th>
                  </tr>
                </thead>

                <tbody>
                    {flatten.map((name) => {
                        return (
                            <tr key={name}>
                                <td>{name}</td>
                                <td>{0}</td>
                                <td>{50}</td>
                                <td>{100}</td>
                            </tr>
                        );
                    })}
                </tbody>
            </table>
        );
    }

    renderWaitMetrics() {
        return (
            <LoadingSpinner>Waiting metrics from node...</LoadingSpinner>
        );
    }

    render() {
        if (!this.props.bench.metrics) {
            return this.renderWaitMetrics();
        }
        console.log("render " + this.props.bench.isRunning());
        return (
            <div>
                {this.renderDownloadReportPanel()}
                {this.props.bench.isRunning() ? this.renderRealtimeMetrics() : this.renderStatMetrics() }
            </div>
        );
    }

    _onChange() {
        this.setState({snapshot: this._getSnapshot(this.props.bench.metrics)});
    }

    _flatten(metrics) {
        return metrics.reduce((a, b) => a.concat(b));
    }

    _getSnapshot(metrics) {
        if (!metrics) return;
        let store = MetricStore.getAll();
        let flatten = this._flatten(metrics);
        return flatten.map((name) => {
            return {name: name, value: store[name]};
        });
    }
}

export default BenchMetrics;

