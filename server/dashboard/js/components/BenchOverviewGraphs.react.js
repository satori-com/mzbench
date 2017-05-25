import React from 'react';
import BenchGraphs from './BenchGraphs.react';
import PropTypes from 'prop-types';

class BenchOverviewGraphs extends React.Component {
    render() {
        return (
            <div>
                <h3>Graphs</h3>
                <BenchGraphs bench={this.props.bench} activeGraph={this.props.activeGraph}/>
            </div>
        );
    }
}

BenchOverviewGraphs.propTypes = {
    bench: PropTypes.object.isRequired
};

export default BenchOverviewGraphs;
