import React from 'react';
import BenchGraphs from './BenchGraphs.react';

class BenchResults extends React.Component {
    render() {
        if (!this.props.bench.results) return null;
        var metricValues = Object.keys(this.props.bench.results).map((key) => {
                let data = this.props.bench.results[key];
                return this.render_metric_result_value(key, data);
            });

        if (metricValues.length > 0) {
            return (
                <div>
                    <h3>Results</h3>
                    <div className="row">
                        {metricValues}
                    </div>
                </div>
            );
        } else {
            return null;
        }
    }

    render_metric_result_value(name, data) {
        if (name.startsWith("systemload.")) return null;
        if (name.startsWith("metric_merging_time")) return null;
        if (name.startsWith("logs.")) return null;
        if (name.startsWith("workers.pool")) return null;

        return (<div key={name} className="col-md-6">
                    <h4 className="bench-results">{name} <small>{data.type}</small> <big>{data.value != undefined ? +data.value.toFixed(2) : null}</big></h4>
                    {this.render_percentiles(data)}
                </div>);
    }

    render_percentiles(data) {
        let decimalPlaces = 2;
        let isCounter = data.type == "counter";
        let percentiles = isCounter ? data.rps : data.percentiles;
        if (percentiles == undefined || Object.keys(percentiles).length == 0) return null;
        return (<table className="table">
                <thead>
                 <tr>
                  <th></th>
                  <th>min</th>
                  <th>P<sub>50</sub></th>
                  <th>P<sub>90</sub></th>
                  <th>P<sub>95</sub></th>
                  <th>max</th>
                  </tr>
                </thead>
                <tbody>
                 <tr>
                  <td className="col-md-2">{isCounter ? "RPS" : "Values"}</td>
                  <td>{+percentiles["min"].toFixed(decimalPlaces)}</td>
                  <td>{+percentiles[50].toFixed(decimalPlaces)}</td>
                  <td>{+percentiles[90].toFixed(decimalPlaces)}</td>
                  <td>{+percentiles[95].toFixed(decimalPlaces)}</td>
                  <td>{+percentiles["max"].toFixed(decimalPlaces)}</td>
                 </tr>
                </tbody>
             </table>);
    }
}

BenchResults.propTypes = {
    bench: React.PropTypes.object.isRequired
};

export default BenchResults;
