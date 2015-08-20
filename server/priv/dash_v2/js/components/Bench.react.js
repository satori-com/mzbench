import React, { PropTypes } from 'react';

import BenchStore from '../stores/BenchStore';
import BenchNav from './BenchNav.react';
import BenchOverview from './BenchOverview.react';
import BenchGraphs from './BenchGraphs.react';
import BenchMetrics from './BenchMetrics.react';
import BenchScenario from './BenchScenario.react';
import BenchLog from './BenchLog.react';
import LoadingSpinner from './LoadingSpinner.react';
import Highlight from './Highlight.react';
import MetricWSConnector from './MetricWSConnector.react';

class Bench extends React.Component {
    constructor(props) {
        super(props);
        this.state = this._resolveState();
        this._onChange = this._onChange.bind(this);
    }

    componentDidMount() {
        BenchStore.onChange(this._onChange);
    }

    componentWillUnmount() {
        BenchStore.off(this._onChange);
    }

    renderActiveTab() {
        let component;
        switch (this.state.tab) {
            case "graphs":
                component = <BenchGraphs bench = {this.state.bench} />;
                break;
            case "metrics":
                component = <BenchMetrics bench = {this.state.bench } />;
                break;
            case "scenario":
                component = <BenchScenario bench = {this.state.bench} />;
                break;
            case "logs":
                component = <BenchLog bench = {this.state.bench} />;
                break;
            default:
                component = <BenchOverview bench = {this.state.bench} />;
                break;

        }
        return component;
    }

    renderLoadingSpinner() {
        return (<LoadingSpinner>Loading...</LoadingSpinner>);
    }

    renderUnknownBench() {
        return (
            <div className="alert alert-warning" role="alert">
                <strong>Oh snap!</strong>&nbsp;
                Cant find benchmark
            </div>
        );
    }

    render() {
        if (!this.state.isLoaded) {
            return this.renderLoadingSpinner();
        }

        if (!this.state.bench) {
            return this.renderUnknownBench();
        }

        return (
            <div key={this.state.bench.id}>
                <MetricWSConnector bench={this.state.bench} />
                <BenchNav bench={this.state.bench} selectedTab={this.state.tab} />
                { this.renderActiveTab() }
            </div>
        );
    }

    _resolveState() {
        if (!BenchStore.isLoaded()) {
            return { isLoaded: false };
        }

        return {
            isLoaded: true,
            bench: BenchStore.getSelectedBench(),
            tab: BenchStore.getActiveTab()
        };
    }

    _onChange() {
        this.setState(this._resolveState());
    }
}

export default Bench;
