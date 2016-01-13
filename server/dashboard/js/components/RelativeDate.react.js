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
        const today = moment();

        if ((today.dayOfYear() == date.dayOfYear()) && (today.year() == date.year())) {
            return date.format("LT");
        }

        if (today.year() == date.year()) {
            return date.format("MMM D");
        }

        return date.format("ll");
    }

    render() {
        const absDate = this._formatAbs(this.props.date);
        const relativeDate = this.props.date.fromNow();

        return (
            <span>{absDate} ({relativeDate})</span>
        );
    }
};

RelativeDate.propTypes = {
    date: React.PropTypes.object.isRequired,
    updateInterval: React.PropTypes.number,
};

RelativeDate.defaultProps = {
    updateInterval: 15000
};

export default RelativeDate;
