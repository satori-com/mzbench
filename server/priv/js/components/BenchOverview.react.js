import React from 'react';

import Duration from './Duration.react';
import BenchSummary from './BenchSummary.react';
import BenchOverviewGraphs from './BenchOverviewGraphs.react';

class BenchOverview extends React.Component {
    render() {
        return (
            <div>
                <Duration bench={this.props.bench} >
                    <BenchSummary bench={this.props.bench} />
                </Duration>
                <hr />
                <BenchOverviewGraphs bench={this.props.bench} />
            </div>
        );
    }
};

BenchOverview.propTypes = {
    bench: React.PropTypes.object.isRequired
};

export default BenchOverview;
