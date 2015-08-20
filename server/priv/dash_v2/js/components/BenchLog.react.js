import React from 'react';

class BenchLog extends React.Component {
    render() {
        const url = '/logs?id='+this.props.bench.id;
        const location = window.location;
        const wsUrl = location.protocol + "//" + location.host + url;

        return (
            <iframe
                frameBorder="0"
                src={url}
                width="100%"
                height="700"
            />
        );
    }
}

BenchLog.propTypes = {
    bench: React.PropTypes.object.isRequired
}

export default BenchLog;
