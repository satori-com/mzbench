import React from 'react';
import moment from 'moment';
import PropTypes from 'prop-types';

class Duration extends React.Component {
    constructor(props) {
        super(props);
        this.state = { duration: this._calculate(props.bench), timeout: undefined };
    }

    componentDidMount() {
        this._createTimer(this.props.bench);
    }

    componentWillUnmount() {
        this._clearTimer();
    }

    componentWillReceiveProps(props) {
        this._clearTimer();
        this._createTimer(props.bench);
    }

    _clearTimer() {
        if (undefined != this.state.timeout) {
            clearTimeout(this.state.timeout);
        }
    }

    _createTimer(bench) {
        let timeout = bench.isRunning() ? setTimeout(() => this._createTimer(bench), 1000) : undefined;
        this.setState({ duration: this._calculate(bench), timeout: timeout });
    }

    _calculate(bench) {
        if (bench.start_time_client) {
            const lastActiveTime = bench.isRunning() ? moment() : bench.finish_time_client;
            return lastActiveTime.diff(bench.start_time_client);
        } else {
            return 0;
        }
    }


    renderChildren() {
        return React.Children.map(this.props.children, (child) => {
            return React.cloneElement(child, { duration: this.state.duration });
        });
    }

    render() {
        return (<div>{this.renderChildren()}</div>);
    }
};

Duration.propTypes = {
    bench: PropTypes.object.isRequired
};

export default Duration;
