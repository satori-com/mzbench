import moment from 'moment';
import React, { PropTypes } from 'react';

export class Duration extends React.Component {
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
        let startTime = moment(bench.startTime);
        let lastActiveTime = bench.isRunning() ? moment() : moment(bench.finishTime);
        return lastActiveTime.diff(startTime);
    }


    renderChildren() {
        return React.Children.map(this.props.children, (child) => {
            return React.cloneElement(child, { duration: this.state.duration });
        });
    }

    render() {
        return (<div>{this.renderChildren()}</div>);
    }
}

Duration.propTypes = {
    bench: React.PropTypes.object.isRequired
};

export default Duration;
