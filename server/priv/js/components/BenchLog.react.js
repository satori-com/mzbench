import React from 'react';

class BenchLog extends React.Component {
    render() {
        const url = '/logs?id='+this.props.bench.id;

        return (
            <div>
                <a href={url} target="_blank">Open in new window</a>
                <iframe
                    frameBorder="0"
                    src={url}
                    width="100%"
                    height="700"
                />
            </div>
        );
    }
};

BenchLog.propTypes = {
    bench: React.PropTypes.object.isRequired
};

export default BenchLog;
