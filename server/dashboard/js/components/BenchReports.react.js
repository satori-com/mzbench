import React from 'react';
import ReactDOM from 'react-dom';
import Modal from './Modal.react';
import PropTypes from 'prop-types';

class BenchReports extends React.Component {
    constructor(props) {
        super(props);
    }

    renderDownloadReportPanel() {
        return (
            <div className="panel panel-default panel-report">
                <div className="panel-heading">
                    <h3 className="panel-title">Download report</h3>
                </div>
                <div className="panel-body btn-toolbar">
                    <a href={`/data?id=${this.props.bench.id}`} target="_blank" className="btn btn-primary" type="submit">Metrics</a>
                    <a href={`/log?id=${this.props.bench.id}`} target="_blank" className="btn btn-primary" type="submit">System logs</a>
                    <a href={`/userlog?id=${this.props.bench.id}`} target="_blank" className="btn btn-primary" type="submit">User logs</a>
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
};

BenchReports.propTypes = {
    bench: PropTypes.object.isRequired
};

export default BenchReports;
