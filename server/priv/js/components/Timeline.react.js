import React, { PropTypes } from 'react';
import BenchStore from '../stores/BenchStore';
import TimelineElement from './TimelineElement.react';
import TimelineFilter from './TimelineFilter.react';
import Duration from './Duration.react';
import LoadingSpinner from './LoadingSpinner.react';
import MZBenchRouter from '../utils/MZBenchRouter'

class Timeline extends React.Component {
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

    renderLoadingSpinner() {
        return (<LoadingSpinner>Loading...</LoadingSpinner>);
    }

    renderClearSearchQueryIfNeeded() {
        const query = BenchStore.getFilter();
        const pager = BenchStore.getPager();

        if (!query && undefined == pager.prev) {
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
        return (
            <div className="alert alert-info" role="alert">
                No results matched your search.
            </div>
        );
    }

    renderTimeline() {
        if (0 == this.state.list.length) {
            return this.renderEmptyTimeline();
        }
        return this.state.list.map((bench) => {
            let isSelected = this.state.selectedBench && this.state.selectedBench.id == bench.id;
            return (
                <Duration key={bench.id} bench={bench}>
                    <TimelineElement key={bench.id} bench={bench} isSelected={isSelected} />
                </Duration>
            );
        });
    }

    renderPrev(boundId) {
        let link = MZBenchRouter.buildLink("#/timeline", { q: BenchStore.getFilter() || undefined, min_id: boundId});
        return (
            <li className="previous"><a href={link}><span aria-hidden="true">&larr;</span> Newer</a></li>
        );
    }

    renderNext(boundId) {
        let link = MZBenchRouter.buildLink("#/timeline", { q: BenchStore.getFilter() || undefined, max_id: boundId});
        return (
            <li className="next"><a href={link}>Older <span aria-hidden="true">&rarr;</span></a></li>
        );
    }

    render() {
        if (!this.state.isLoaded) {
            return this.renderLoadingSpinner();
        }

        let pager = BenchStore.getPager();
        return (
            <div>
                <TimelineFilter filter={BenchStore.getFilter()}/>
                {this.renderClearSearchQueryIfNeeded()}
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
        if (!BenchStore.isLoaded()) {
            return { isLoaded: false };
        }

        return {
            selectedBench: BenchStore.getSelectedBench(),
            list: BenchStore.getBenchmarks(),
            isLoaded: true
        };
    }

    _onChange() {
        this.setState(this._resolveState());
    }
}

export default Timeline;
