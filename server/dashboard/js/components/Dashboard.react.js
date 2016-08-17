import React from 'react';

import DashboardStore from '../stores/DashboardStore';
import DashboardEdit from './DashboardEdit.react';
import DashboardOverview from './DashboardOverview.react';
import DashboardNav from './DashboardNav.react';
import LoadingSpinner from './LoadingSpinner.react';

class Dashboard extends React.Component {
    constructor(props) {
        super(props);
        this.state = this._resolveState();
        this._onChange = this._onChange.bind(this);
    }

    componentDidMount() {
        DashboardStore.onChange(this._onChange);
    }

    componentWillUnmount() {
        DashboardStore.off(this._onChange);
    }

    renderActiveTab() {
        let component;
        switch (this.state.tab) {
            case "settings":
                component = <DashboardEdit item = {this.state.dashboard } />;
                break;
            default:
                component = <DashboardOverview item = {this.state.dashboard} activeGraph = {this.state.activeGraph}
                                               benchset = {this.state.benchset} benchsetId = {this.state.benchsetId}/>;
                break;

        }
        return component;
    }

    renderLoadingSpinner() {
        return (<LoadingSpinner>Loading...</LoadingSpinner>);
    }

    renderUnknownDashboard() {
        return (
            <div className="alert alert-warning" role="alert">
                <strong>Oh snap!</strong>&nbsp;
                Cant find dashboard
            </div>
        );
    }

    render() {
        if (!this.state.isLoaded) {
            return this.renderLoadingSpinner();
        }

        if (this.state.isNewSelected) {
            return <DashboardEdit item={DashboardStore.getNew()} />;
        }

        if (!this.state.dashboard) {
            return this.renderUnknownDashboard();
        }

        return (
            <div key={this.state.dashboard.id}>
                <DashboardNav item={this.state.dashboard} selectedTab={this.state.tab} />
                { this.renderActiveTab() }
            </div>
        );
    }

    _resolveState() {
        if (!DashboardStore.isLoaded()) {
            return { isLoaded: false };
        }

        if (DashboardStore.isNewSelected()) {
            return { isLoaded: true, isNewSelected: true };
        }

        return {
            isLoaded: true,
            isNewSelected: false,
            dashboard: DashboardStore.getSelected(),
            tab: DashboardStore.getActiveTab(),
            activeGraph: DashboardStore.getSelectedGraph(),
            benchset : DashboardStore.getBenchset(),
            benchsetId: DashboardStore.getBenchsetId()
        };
    }

    _onChange() {
        this.setState(this._resolveState());
    }
}

export default Dashboard;
