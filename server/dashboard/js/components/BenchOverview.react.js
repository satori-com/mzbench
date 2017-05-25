import React from 'react';

import Duration from './Duration.react';
import BenchSummary from './BenchSummary.react';
import BenchOverviewGraphs from './BenchOverviewGraphs.react';
import BenchResults from './BenchResults.react';
import PropTypes from 'prop-types';

class BenchOverview extends React.Component {
    render() {
        return (
            <div>
                <Duration bench={this.props.bench} >
                    <BenchSummary bench={this.props.bench} />
                </Duration>
                <hr />
                <BenchOverviewGraphs bench={this.props.bench} activeGraph={this.props.activeGraph}/>
                <BenchResults bench={this.props.bench}/>
            </div>
        );
    }
};

BenchOverview.propTypes = {
    bench: PropTypes.object.isRequired
};

export default BenchOverview;
