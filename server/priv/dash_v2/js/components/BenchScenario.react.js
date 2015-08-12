import React, { PropTypes } from 'react';
import Highlight from './Highlight.react';

class BenchScenario extends React.Component {
    render() {
        return (
            <Highlight className="erlang">
                {this.props.bench.scenarioBody}
            </Highlight>
        );
    }
}


export default BenchScenario;
