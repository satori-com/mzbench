import React from 'react';
import moment from 'moment';

import RelativeDate from './RelativeDate.react';
import MZBenchActions from '../actions/MZBenchActions';

class TimelineElement extends React.Component {
    render() {
        let { bench, isSelected, duration } = this.props;

        let cssClass = "bs bs-" + (bench.isRunning() ? "progress" :  bench.status);

        if (isSelected) {
            cssClass += " bs-selected";
        }

        return (
            <a href={`#/bench/${bench.id}/overview`} className="bs-link">
                <div className={cssClass}>
                    <h6 className="no-overflow">
                        #{bench.id} {bench.benchmark_name}
                        {bench.isRunning() ? <span className="label">{bench.status}</span> : null}
                    </h6>
                    <div><i className="glyphicon glyphicon-time"></i> {moment.duration(duration).humanize()}</div>
                    <div><i className="glyphicon glyphicon-calendar"></i> <RelativeDate date = {bench.start_time_client} /></div>
                </div>
            </a>
        );
    }

    _onClick() {
        MZBenchActions.selectBenchById(this.props.bench.id);
    }
}

TimelineElement.propTypes = {
    bench: React.PropTypes.object.isRequired,
    isSelected: React.PropTypes.bool
};

TimelineElement.defaultProps = {
    isSelected: false
};

export default TimelineElement;
