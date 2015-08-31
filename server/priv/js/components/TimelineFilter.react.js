import React from 'react';
import MZBenchRouter from '../utils/MZBenchRouter';
import BenchStore from '../stores/BenchStore';

class TimelineFilter extends React.Component {
    constructor(props) {
        super(props);
        this._onKeyDown = this._onKeyDown.bind(this);
        this._onChange = this._onChange.bind(this);
        this.state = {filter: this.props.filter};
        this.autoSearchInterval = 500;
        this.autoSearchHandler = null;
    }

    componentWillReceiveProps(nextProps) {
        if (this.props.filter != nextProps.filter) {
            this.setState({filter: nextProps.filter});
        }
    }

    render() {
        return (
            <form>
                <div className="form-group">
                    <div className="input-group">
                        <div className="input-group-addon">Filter</div>
                        <input type="text" ref="filterInput" className="form-control" placeholder="Search Benchmarks" onKeyDown={this._onKeyDown} value={this.state.filter} onChange={this._onChange} />
                    </div>
                </div>
            </form>
        );
    }

    _onKeyDown(event) {
        var runSearch = () => MZBenchRouter.navigate("/timeline", {q: this.state.filter});
        if (event.key === 'Enter') {
            event.preventDefault();
            runSearch();
        } else {
            if (this.autoSearchHandler) clearTimeout(this.autoSearchHandler);
            this.autoSearchHandler = setTimeout(runSearch, this.autoSearchInterval);
        }
    }

    _onChange(event) {
        this.setState({filter: event.target.value});
    }
};

export default TimelineFilter;
