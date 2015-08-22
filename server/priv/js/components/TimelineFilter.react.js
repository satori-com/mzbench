import React from 'react';
import MZBenchRouter from '../utils/MZBenchRouter';
import BenchStore from '../stores/BenchStore';

class TimelineFilter extends React.Component {
    constructor(props) {
        super(props);
        this._onKeyDown = this._onKeyDown.bind(this);
        this._onChange = this._onChange.bind(this);
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
                        <input type="text" ref="filterInput" className="form-control" placeholder="Search Benchmarks" onKeyDown={this._onKeyDown} value={this.state.filter} onChange={this._onChange} />
                    </div>
                </div>
            </form>
        );
    }

    _onKeyDown(event) {
        if (event.key === 'Enter') {
            event.preventDefault();
            MZBenchRouter.navigate("/timeline", {q: this.state.filter});
        }
    }

    _onChange(event) {
        this.setState({filter: event.target.value});
    }
};

export default TimelineFilter;
