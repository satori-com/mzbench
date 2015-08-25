import React from 'react';
import Highlight from './Highlight.react';

class BenchScenario extends React.Component {
    render() {
        return (
            <Highlight className="erlang">
                {this.props.bench.script_body || ""}
            </Highlight>
        );
    }
};

BenchScenario.propTypes = {
    bench: React.PropTypes.object.isRequired
};

export default BenchScenario;
