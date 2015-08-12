import React, { PropTypes } from 'react';
import BenchStore from '../stores/BenchStore';
import TimelineElement from './TimelineElement.react';
import Duration from './Duration.react';
import LoadingSpinner from './LoadingSpinner.react';

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

    render() {
        if (!this.state.isLoaded) {
            return this.renderLoadingSpinner();
        }

        return (
            <div> {
                this.state.list.map((bench) => {
                    let isSelected = this.state.selectedBench && this.state.selectedBench.id == bench.id;
                    return (
                        <Duration key={bench.id} bench={bench}>
                            <TimelineElement key={bench.id} bench={bench} isSelected={isSelected} />
                        </Duration>
                    );
                })}
                <nav>
                    <ul className="pager">
                        <li><a href="#">Previous</a></li>&nbsp;
                        <li><a href="#">Next</a></li>
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
