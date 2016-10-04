import React from 'react';
import MZBenchActions from '../actions/MZBenchActions';
import Timeline from './Timeline.react';
import Details from './Details.react';
import Auth from './Auth.react';

class MZBenchApp extends React.Component {

    componentWillMount() {
        MZBenchActions.subscribeBenchTimeline();
    }

    componentWillUnmount() {
        MZBenchActions.unsubscribeBenchTimeline();
    }

    render() {
        return (
            <div className="fluid-container">
                <Auth title="Please Sign-in" />
                <div className="row">
                    <div className="col-xs-3 timeline-pane">
                        <Timeline />
                    </div>
                    <div className="col-xs-8 bench-pane">
                        <Details />
                    </div>
                </div>
            </div>
        );
    }
};

export default MZBenchApp;
