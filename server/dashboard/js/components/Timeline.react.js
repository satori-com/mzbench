import React from 'react';
import BenchStore from '../stores/BenchStore';
import DashboardStore from '../stores/DashboardStore';
import GlobalStore from '../stores/GlobalStore';
import TimelineElement from './TimelineElement.react';
import TimelineFilter from './TimelineFilter.react';
import Duration from './Duration.react';
import LoadingSpinner from './LoadingSpinner.react';
import MZBenchRouter from '../utils/MZBenchRouter'
import MZBenchActions from '../actions/MZBenchActions';

class Timeline extends React.Component {
    constructor(props) {
        super(props);
        this.state = this._resolveState();
        this._onChange = this._onChange.bind(this);
    }

    componentDidMount() {
        BenchStore.onChange(this._onChange);
        GlobalStore.onChange(this._onChange);
        DashboardStore.onChange(this._onChange);

        if (GlobalStore.isDashboardModeOn())
            MZBenchActions.getDashboards();
        else
            MZBenchActions.getTimeline();

        MZBenchActions.getServerInfo();
    }

    componentWillUnmount() {
        BenchStore.off(this._onChange);
        GlobalStore.off(this._onChange);
        DashboardStore.off(this._onChange);
    }

    renderLoadingSpinner() {
        return (<LoadingSpinner>Loading...</LoadingSpinner>);
    }

    renderNewIfNeeded() {
        if (!this.state.isNewActive) return null;

        let cssClass = "bs bs-new";

        if (this.state.isNewSelected) {
            cssClass += " bs-selected";
        }

        let link = this.state.dashboardMode ? "#/dashboard/new" : "#/new";

        return (
            <a href={link} className="bs-link">
                <div className={cssClass}>
                    <h6 className="no-overflow">
                        {this.state.newName}
                        <span className="label">new</span>
                    </h6>
                </div>
            </a>
        );
    }

    renderClearSearchQueryIfNeeded() {
        const pager = this.state.pager;

        if (!this.state.filter && undefined == pager.prev) {
            return null;
        }

        let link = MZBenchRouter.buildLink("#/timeline", {});
        return (
            <div className="reset-query">
                <a href={link}>
                    <span className="glyphicon glyphicon-remove-sign"></span> Clear search query and pagination
                </a>
            </div>
        );
    }

    renderEmptyTimeline() {
        if (this.state.isTimelineLoading || this.state.filter) {
            return (
                <div className="alert alert-info" role="alert">
                    No results matched your search.
                </div>
            );
        }

        if (this.state.dashboardMode)
            return (
                <div className="alert alert-info" role="alert">
                    There aren't any dashboards.
                    Press <b>&quot;+&quot;</b> above to create one.
                </div>
            );

        return (
            <div className="alert alert-info" role="alert">
                There aren't any benchmarks.
                Use <a href="https://github.com/machinezone/mzbench#quickstart" target="_blank"><strong>Quickstart guide</strong></a> to create some.
            </div>
        );
    }

    renderTimelineBody() {
        if (0 == this.state.list.length) {
            return this.renderEmptyTimeline();
        }

        if (GlobalStore.isDashboardModeOn()) {
            return this.state.list.map((item) => {
                let isSelected = this.state.selectedItem && this.state.selectedItem.id == item.id;
                return (<TimelineElement key={item.id} bench={item} isSelected={isSelected} />);
            });
        } else {
            return this.state.list.map((item) => {
                let isSelected = this.state.selectedItem && this.state.selectedItem.id == item.id;
                return (
                    <Duration key={item.id} bench={item}>
                        <TimelineElement key={item.id} bench={item} isSelected={isSelected} />
                    </Duration>
                );
            });
        }
    }

    renderTimeline() {
        return (
            <div className="timeline-body">
                { this.state.isTimelineLoading ? <div className="load-mask" /> : null }
                { this.renderTimelineBody() }
            </div>
        );
    }

    renderPrev(boundId) {
        let link = MZBenchRouter.buildLink("#/timeline", { q: this.state.filter || undefined, min_id: boundId});
        return (
            <li className="previous"><a href={link}><span aria-hidden="true">&larr;</span> Newer</a></li>
        );
    }

    renderNext(boundId) {
        let link = MZBenchRouter.buildLink("#/timeline", { q: this.state.filter || undefined, max_id: boundId});
        return (
            <li className="next"><a href={link}>Older <span aria-hidden="true">&rarr;</span></a></li>
        );
    }

    render() {
        if (!this.state.isLoaded) {
            return this.renderLoadingSpinner();
        }

        const pager = this.state.pager;

        return (
            <div>
                <TimelineFilter filter={this.state.filter} dashboardMode={this.state.dashboardMode}/>
                {this.renderClearSearchQueryIfNeeded()}
                {this.renderNewIfNeeded()}
                {this.renderTimeline()}
                <nav>
                    <ul className="pager">
                        {(undefined !== pager.prev) ? this.renderPrev(pager.prev) : null }
                        {(undefined !== pager.next) ? this.renderNext(pager.next) : null }
                    </ul>
                </nav>
            </div>
        );
    }

    _resolveState() {
        let dashboardMode = GlobalStore.isDashboardModeOn();
        let store = dashboardMode ? DashboardStore : BenchStore;

        if (!store.isLoaded()) {
            return { isLoaded: false };
        }

        return {
            selectedItem: store.getSelected(),
            filter: store.getFilter(),
            pager: store.getPager(),
            list: store.getItems(),
            isNewActive: store.isNewActive(),
            isNewSelected: store.isNewSelected(),
            newName: store.getNew().name,
            isTimelineLoading: store.isShowTimelineLoadingMask(),
            dashboardMode : dashboardMode,
            isLoaded: true
        };
    }

    _onChange() {
        this.setState(this._resolveState());
    }
};

export default Timeline;
