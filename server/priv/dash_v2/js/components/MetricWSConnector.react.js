import React, { PropTypes } from 'react';
import MZBenchActions, { spawn } from '../actions/MZBenchActions';

export class MetricWSConnector extends React.Component {
    constructor(props) {
        super(props);
        this.ws = undefined;
    }

    // all non-user actions in react callbacks should be asynced to avoid double dispatching
    componentDidMount() {
        spawn(() => {
            MZBenchActions.resetMetrics();
            if (this.props.bench.isRunning()) {
                this.ws = MZBenchActions.subscribeMetrics(this.props.bench.id);
            }
        });
    }

    componentWillUnmount() {
        spawn(() => {
            if (this.ws) {
                MZBenchActions.unsubscribeMetrics(this.ws);
                this.ws = undefined;
            }
        });
    }

    componentWillReceiveProps(props) {
        spawn(() => {
            let isRunning = props.bench.isRunning();

            if (!isRunning && this.ws) {
                MZBenchActions.unsubscribeMetrics(this.ws);
                this.ws = undefined;
            }
        });
    }

    render() {
        return false;
    }
}

MetricWSConnector.propTypes = {
    bench: React.PropTypes.object.isRequired
}

export default MetricWSConnector;
