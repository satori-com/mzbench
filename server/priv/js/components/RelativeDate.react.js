import React from 'react';
import moment from 'moment';

class RelativeDate extends React.Component {
    componentDidMount() {
        this.timer = setInterval(() => this.forceUpdate(), this.props.updateInterval);
    }

    componentWillUnmount() {
        clearInterval(this.timer);
    }

    _formatAbs(date) {
        const target = moment(date);
        const today = moment();

        if ((today.dayOfYear() == target.dayOfYear()) && (today.year() == target.year())) {
            return target.format("LT");
        }

        if (today.year() == target.year()) {
            return target.format("MMM D");
        }

        return target.format("ll");
    }

    render() {
        const absDate = this._formatAbs(this.props.date);
        const relativeDate = moment(this.props.date).fromNow();

        return (
            <span>{absDate} ({relativeDate})</span>
        );
    }
};

RelativeDate.propTypes = {
    date: React.PropTypes.string.isRequired,
    updateInterval: React.PropTypes.number,
};

RelativeDate.defaultProps = {
    updateInterval: 15000
};

export default RelativeDate;
