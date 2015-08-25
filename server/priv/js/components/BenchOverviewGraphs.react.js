import React from 'react';
import BenchGraphs from './BenchGraphs.react';

class BenchOverviewGraphs extends React.Component {
    render() {
        return (
            <div>
                <h3>Graphs</h3>
                <BenchGraphs bench={this.props.bench}/>
            </div>
        );
    }
}

BenchOverviewGraphs.propTypes = {
    bench: React.PropTypes.object.isRequired
};

export default BenchOverviewGraphs;
