import React from 'react';
import ReactDOM from 'react-dom';
import Highlight from './Highlight.react';
import MZBenchRouter from '../utils/MZBenchRouter';

class BenchScenario extends React.Component {
    render() {
        let env = this.props.bench.env;
        let editable = this.props.bench.status == "running";
        var envForm = env.map((kv, index) => {
                return (
                    <div key={kv.name} className="col-md-6 form-group">
                        <label className="control-label">{kv.name}</label>
                        <input type="text" ref={kv.name} name={kv.name} defaultValue={kv.value} className="form-control" readOnly={ editable ? null : "readonly"}></input>
                    </div>
                );
            });

        return (
            <div>
                { env.length > 0 ?
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
                        <Highlight className="python">
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

        query += this.props.bench.env.map(
            (x) => {
                let oldValue = x.value;
                let newValue = ReactDOM.findDOMNode(this.refs[x.name]).value;
                if (newValue != oldValue) {
                    return "&" + x.name.toString() + "=" + newValue;
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
            beforeSend: function (xhr) {
                if (AuthStore.getToken()) {
                    xhr.setRequestHeader("Authorization", "Bearer " + AuthStore.getToken() );
                }
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
