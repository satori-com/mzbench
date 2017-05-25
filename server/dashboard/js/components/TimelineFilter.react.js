import React from 'react';
import MZBenchRouter from '../utils/MZBenchRouter';
import MZBenchActions from '../actions/MZBenchActions';
import BenchStore from '../stores/BenchStore';
import PropTypes from 'prop-types';

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
        let placeholder = this.props.dashboardMode ? "Search Dashboards" : "Search Benchmarks";
        return (
            <form>
                <div className="form-group">
                    <div className="input-group">
                        <input type="text" ref="filterInput" className="form-control" placeholder={placeholder} onKeyDown={this._onKeyDown.bind(this)} value={this.state.filter} onChange={this._onChange.bind(this)} />
                        {
                            this.props.dashboardMode ?
                                (<div className="input-group-btn">
                                    <a role="button" className="btn btn-danger" href="#/" title="Turn off dashboard mode"><span className="glyphicon glyphicon-signal"></span></a>
                                    <a role="button" className="btn btn-success" href="#/dashboard/new"><span className="glyphicon glyphicon-plus"></span></a>
                                </div>) :
                                (<div className="input-group-btn">
                                    <a role="button" className="btn btn-info" href="#/dashboard" title="Turn on dashboard mode"><span className="glyphicon glyphicon-dashboard"></span></a>
                                    <a role="button" className="btn btn-success" href="#/new"><span className="glyphicon glyphicon-plus"></span></a>
                                </div>)
                        }
                    </div>
                </div>
            </form>
        );
    }

    _runSearch() {
        MZBenchRouter.navigate((this.props.dashboardMode ? "/dashboard" : "") + "/timeline", {q: this.state.filter});
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
    filter: PropTypes.string,
    dashboardMode: PropTypes.bool,
    autoSearchInterval: PropTypes.number
};

TimelineFilter.defaultProps = {
    autoSearchInterval: 500
};

export default TimelineFilter;
