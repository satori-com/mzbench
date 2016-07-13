import React from 'react';
import BenchGraphs from './BenchGraphs.react';

class BenchResults extends React.Component {
    render() {
    	if (!this.props.bench.results) return null;
        var envForm = Object.keys(this.props.bench.results).map((key) => {
                return (
                    <div key={key} className="col-md-6 form-group">
                        <label className="control-label">{key}</label>
                        <input type="text" ref={key} defaultValue={this.props.bench.results[key]} className="form-control" readOnly="readonly"/>
                    </div>
                );
            });

        return (
            <div>
                <h3>Results</h3>
                <div className="row">
                    <form>
                        {envForm}
                    </form>
                 </div>
            </div>
        );
    }
}

BenchResults.propTypes = {
    bench: React.PropTypes.object.isRequired
};

export default BenchResults;
