import React from 'react';
import Bench from './Bench.react';
import Dashboard from './Dashboard.react';

import GlobalStore from '../stores/GlobalStore';

class Details extends React.Component {
    constructor(props) {
        super(props);
        this.state = this._resolveState();
        this._onChange = this._onChange.bind(this);
    }

    componentDidMount() {
        GlobalStore.onChange(this._onChange);
    }

    componentWillUnmount() {
        GlobalStore.off(this._onChange);
    }

    render() {
        if (!this.state.dashboardMode) {
            return (<Bench />);
        } else {
            return (<Dashboard />);
        }
    }

    _resolveState() {
        return { dashboardMode: GlobalStore.isDashboardModeOn() };
    }

    _onChange() {
        this.setState(this._resolveState());
    }
}

export default Details;
