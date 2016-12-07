import React from 'react';
import Timeline from './Timeline.react';
import Details from './Details.react';
import Auth from './Auth.react';

class MZBenchApp extends React.Component {
    render() {
        return (
            <div className="fluid-container">
                <Auth title="Sign In">
                    <div className="row">
                        <div className="col-xs-3 timeline-pane">
                            <Timeline />
                        </div>
                        <div className="col-xs-8 bench-pane">
                            <Details />
                        </div>
                    </div>
                </Auth>
            </div>
        );
    }
};

export default MZBenchApp;
