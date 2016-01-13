import React from 'react';
import Highlight from './Highlight.react';
import MZBenchRouter from '../utils/MZBenchRouter';

class BenchScenario extends React.Component {
    render() {
        let env = this.props.bench.env;
        let editable = this.props.bench.status == "running";
        var envForm = Object.keys(env).map((key) => {
                return (
                    <div key={key} className="col-md-6 form-group">
                        <label className="control-label">{key}</label>
                        <input type="text" ref={key} name={key} defaultValue={env[key]} className="form-control" readOnly={ editable ? null : "readonly"}></input>
                    </div>
                );
            });

        return (
            <div>
                { Object.keys(env).length > 0 ?
                    (<div className="row">
                        <form>
                            {envForm}
                            <div className="col-md-12 form-group text-right">
                                <button onClick={this._onEnvSave.bind(this)} className="btn btn-success" disabled={ editable ? null : "disabled"}>Save</button>
                            </div>
                        </form>
                     </div>) : "" }
                <div className="row">
                    <div className="col-md-12">
                        <Highlight className="erlang">
                            {this.props.bench.script_body || ""}
                        </Highlight>
                    </div>
                </div>
            </div>
        );

    }

    _onEnvSave(event) {
        event.preventDefault();
        let notify = $.notify({message: `Saving environment... `}, {type: 'info', delay: 0});
        let query = MZBenchRouter.buildLink('/change_env', {id: this.props.bench.id});

        query += Object.keys(this.props.bench.env).map(
            (x) => {
                let oldValue = this.props.bench.env[x];
                let newValue = React.findDOMNode(this.refs[x]).value;
                if (newValue != oldValue) {
                    return "&" + x.toString() + "=" + newValue;
                } else {
                    return ""
                }
            }).join("");

        $.ajax({url: query, type : 'GET',
            processData: false,
            contentType: false,
            success: (data) => {
                notify.update({message: `Environment has been updated`, type: 'success'});
                setTimeout(() => notify.close(), 5000);
            },
            error: () => {
                notify.update({message: `Failed to save new environment`, type: 'danger'});
                setTimeout(() => notify.close(), 5000);
            }});
    }

};

BenchScenario.propTypes = {
    bench: React.PropTypes.object.isRequired
};

export default BenchScenario;
