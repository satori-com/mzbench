import React, { PropTypes } from 'react';
import LoadingSpinner from './LoadingSpinner.react';
import MetricStore from '../stores/MetricStore';

class BenchOverviewMetrics extends React.Component {
    constructor(props) {
        super(props);
        let metrics = this._getRealtimeMetrics(props.bench);
        this.state = { metrics: metrics, snapshot: this._getSnapshot(metrics) };
        this._onChange = this._onChange.bind(this);
    }

    componentDidMount() {
        MetricStore.onChange(this._onChange);
    }

    componentWillUnmount() {
        MetricStore.off(this._onChange);
    }

    renderWaitMetrics() {
        return (
            <LoadingSpinner>Waiting metrics from node...</LoadingSpinner>
        );
    }

    renderDataTable() {
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

    render() {
        if (!this.props.bench.isRunning()) {
            return false;
        }

        let hasMetrics = this.state.metrics

        let component;

        if (hasMetrics) {
            component = this.renderDataTable();
        } else {
            component = this.renderWaitMetrics();
        }

        return (
            <div>
                <h3>Real-time metrics</h3>
                { component }
            </div>
        );
    }

    componentWillReceiveProps(props) {
        let metrics = this._getRealtimeMetrics(props.bench);
        this.setState({ metrics: metrics,
                        snapshot: this._getSnapshot(metrics) });
    }

    _onChange() {
        this.setState({snapshot: this._getSnapshot(this.state.metrics)});
    }


    _getSnapshot(metrics) {
        if (!metrics) return;
        let store = MetricStore.getAll();
        return metrics.map((name) => {
            return {name: name, value: (undefined !== store[name] ? store[name] : "-")};
        });
    }

    _getRealtimeMetrics(bench) {
        // FIXME for demo pick first metric from each group
        // demo implementation
        if (!bench || !bench.metrics) return;
        return bench.metrics.slice(0, this.props.limit).map((group) => group[0]);
    }
}

BenchOverviewMetrics.propTypes = { bench: React.PropTypes.object.isRequired,
                                   limit: React.PropTypes.number};
BenchOverviewMetrics.defaultProps = { limit: 8 };

export default BenchOverviewMetrics;
