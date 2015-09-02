import React from 'react';
import MZBenchRouter from '../utils/MZBenchRouter';
import MZBenchActions from '../actions/MZBenchActions';
import BenchStore from '../stores/BenchStore';

class TimelineFilter extends React.Component {
    constructor(props) {
        super(props);
        this.autoSearchHandler = null;
        this.state = {filter: this.props.filter};
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
                        <input type="text" ref="filterInput" className="form-control" placeholder="Search Benchmarks" onKeyDown={this._onKeyDown.bind(this)} value={this.state.filter} onChange={this._onChange.bind(this)} />
                    </div>
                </div>
            </form>
        );
    }

    _runSearch() {
        MZBenchRouter.navigate("/timeline", {q: this.state.filter});
    }

    _onKeyDown(event) {
        if (event.key === 'Enter') {
            event.preventDefault();
            this._runSearch();
        }
    }

    _onChange(event) {
        this.setState({filter: event.target.value});

        if (this.autoSearchHandler) {
            clearTimeout(this.autoSearchHandler);
        }
        this.autoSearchHandler = setTimeout(() => this._runSearch(), this.props.autoSearchInterval);
    }
};

TimelineFilter.propTypes = {
    filter: React.PropTypes.string,
    autoSearchInterval: React.PropTypes.number
};

TimelineFilter.defaultProps = {
    autoSearchInterval: 500
};

export default TimelineFilter;
