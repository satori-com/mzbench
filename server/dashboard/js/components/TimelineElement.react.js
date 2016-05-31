import React from 'react';
import moment from 'moment';

import RelativeDate from './RelativeDate.react';
import MZBenchActions from '../actions/MZBenchActions';
import Star from './Star.react';
import MZBenchRouter from '../utils/MZBenchRouter';

class TimelineElement extends React.Component {
    render() {
        let { bench, isSelected, duration } = this.props;

        let cssClass = "bs bs-" + (bench.isRunning() ? "progress" :  bench.status);

        if (isSelected) {
            cssClass += " bs-selected";
        }

        var tags = bench.tags.length <= 0 ? [] :
                <div className="timeline-tags no-overflow">
                {bench.tags.map(
                    (t, i) => {
                        return <span key={i}>
                                <span className={isSelected?"timeline-tag-link-selected":"timeline-tag-link"}
                                      onClick={(e) => { return this._onTagClick(t, e) }} key={i}>{"#"+t}
                                </span>
                                &nbsp;
                               </span>
                    })}
                </div>;

        return (
            <a href={`#/bench/${bench.id}/overview`} className="bs-link">
                <div className={cssClass}>
                    <h6 className="no-overflow">
                        #{bench.id} {bench.benchmark_name}
                        {bench.isRunning() ? <span className="label">{bench.status}</span> : null}
                            <Star selected={bench.tags.indexOf("favorites") > -1} onClick={(v) => {
                                if (v == true) MZBenchActions.addBenchTag(bench.id, "favorites");
                                else MZBenchActions.removeBenchTag(bench.id, "favorites");
                            }}/>
                    </h6>
                    {tags}
                    <div><i className="glyphicon glyphicon-time"></i> {moment.duration(duration).humanize()}</div>
                    <div><i className="glyphicon glyphicon-calendar"></i> <RelativeDate date = {bench.start_time_client} /></div>
                </div>
            </a>
        );
    }

    _onTagClick(tag, event) {
        event.preventDefault();
        MZBenchRouter.navigate("/timeline", {q: "#"+tag});
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
