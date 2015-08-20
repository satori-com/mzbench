import React, { PropTypes } from 'react';

import LoadingSpinner from './LoadingSpinner.react';

class BenchMetrics extends React.Component {
    renderDownloadReportPanel() {
        return (
            <div className="panel panel-default panel-report">
                <div className="panel-heading">
                    <h3 className="panel-title">Download report&nbsp;
                    {this.props.bench.isRunning() ? <small>(enabled for finished benches only)</small> : null}
                    </h3>
                </div>
                <div className="panel-body">
                    <button className="btn btn-primary" type="submit" disabled={this.props.bench.isRunning()}>Email</button>
                    <button className="btn btn-primary" type="submit" disabled={this.props.bench.isRunning()}>Text</button>
                    <button className="btn btn-primary" type="submit" disabled={this.props.bench.isRunning()}>CSV</button>
                    <button className="btn btn-primary" type="submit" disabled={this.props.bench.isRunning()}>JSON</button>
                </div>
            </div>
        );
    }


    render() {
        return (
            <div>
                {this.renderDownloadReportPanel()}
            </div>
        );
    }

}

BenchMetrics.propTypes = {
    bench: React.PropTypes.object.isRequired
}

export default BenchMetrics;
