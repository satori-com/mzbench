import React, { PropTypes } from 'react';

import moment from 'moment';
import moment_df from 'moment-duration-format';

class BenchSummary extends React.Component {
    constructor(props) {
        super(props);
    }

    _labelCssClass(status) {
        switch (status) {
            case "complete":
                return "label-success";
            case "failed":
                return "label-danger";
            case "stopped":
                return "label-default";
        }
        return "label-info";
    }

    render() {
        let bench = this.props.bench;

        let labelClass = this._labelCssClass(bench.status);

        return (
            <div className="fluid-container">
                <div className="row bench-details">
                    <div className="col-md-6">
                        <table className="table">
                            <tbody>
                                <tr>
                                    <th scope="row" className="col-md-2">Scenario</th>
                                    <td>#{bench.id} {bench.scenario}</td>
                                </tr>
                                <tr>
                                    <th scope="row">Duration</th>
                                    <td>{moment.duration(this.props.duration).format("h [hrs], m [min], s [sec]")}</td>
                                </tr>
                                <tr>
                                    <th scope="row">Date</th>
                                    <td>{moment(bench.start_time).format("lll")}</td>
                                </tr>
                                <tr>
                                    <th scope="row">Status</th>
                                    <td><span className={`label ${labelClass}`}>{bench.status}</span></td>
                                </tr>
                            </tbody>
                        </table>
                    </div>

                    <div className="bench-actions col-md-offset-2 col-md-4">
                        <div className="text-right">
                            <a type="button" className="btn btn-sm btn-danger" href="#/stop" disabled={!this.props.bench.isRunning()}>
                                <span className="glyphicon glyphicon-minus-sign"></span> Stop
                            </a>
                        </div>
                        <div className="text-right">
                            <a type="button" className="btn btn-sm btn-primary disable" href="#/restart">
                                <span className="glyphicon glyphicon-refresh"></span> Restart
                            </a>
                        </div>
                    </div>
                </div>
            </div>
        );
    }
}

BenchSummary.propTypes = {
    bench: React.PropTypes.object.isRequired
};

export default BenchSummary;
