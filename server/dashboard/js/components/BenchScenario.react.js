import React from 'react';
import ReactDOM from 'react-dom';
import Highlight from './Highlight.react';
import MZBenchRouter from '../utils/MZBenchRouter';
import AuthStore from '../stores/AuthStore';

const PERCENTS = [100, 75, 66, 50, 33, 25, 15, 10, 7, 5, 1];
const POOLS = [1, 2, 3, 4, 5];

class BenchScenario extends React.Component {
    constructor(props) {
            super(props);
            this._onRunCommand = this._onRunCommand.bind(this);
            this._onChangeCommand = this._onChangeCommand.bind(this);
            this._onChangePool = this._onChangePool.bind(this);
            this._onChangePercent = this._onChangePercent.bind(this);
            this.state = this._resolveState();
    }
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
                { editable ? (<div className="row">
                    <div className="col-md-2 form-group">
                        <select className="form-control" defaultValue={this.state.form.pool} onChange={this._onChangePool}>{POOLS.map(this.renderPoolOption)}</select>
                    </div>
                    <div className="col-md-3 form-group">
                        <select className="form-control" defaultValue={this.state.form.percent} onChange={this._onChangePercent}>{PERCENTS.map(this.renderPercentOption)}</select>
                    </div>
                    <div className="col-md-5 form-group">
                        <input type="text" defaultValue={this.state.form.command} className="form-control" onChange={this._onChangeCommand} placeholder="worker_command()" />
                    </div>
                    <div className="col-md-1 form-group">
                        <button onClick={this._onRunCommand} className="btn btn-success">Run</button>
                    </div>
                </div>) : null}
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

    renderPoolOption(value, idx) {
        return (<option key={idx} value={value}>pool{value}</option>);
    }

    renderPercentOption(value, idx) {
        return (<option key={idx} value={value}>{value}% of workers</option>);
    }

    _onChangeCommand(event) {
        let state = this.state;
        state.form.command = event.target.value;
        this.setState(state);
    }

    _onChangePercent(event) {
        let state = this.state;
        state.form.percent = event.target.value;
        this.setState(state);
    }

    _onChangePool(event) {
        let state = this.state;
        state.form.pool = event.target.value;
        this.setState(state);
    }

    _runAjax(query, successMessage, failMessage, notify) {
        $.ajax({url: query, type : 'GET',
            processData: false,
            contentType: false,
            success: (data) => {
                notify.update({message: successMessage, type: 'success'});
                setTimeout(() => notify.close(), 5000);
            },
            beforeSend: function (xhr) {
                if (AuthStore.getToken()) {
                    xhr.setRequestHeader("Authorization", "Bearer " + AuthStore.getToken() );
                }
            },
            error: () => {
                notify.update({message: failMessage, type: 'danger'});
                setTimeout(() => notify.close(), 5000);
            }});
    }

    _onRunCommand(event) {
        event.preventDefault();
        let notify = $.notify({message: `Executing... `}, {type: 'info', delay: 0});
        let query = MZBenchRouter.buildLink('/run_command', {id: this.props.bench.id}) +
                    "&pool=" + this.state.form.pool + "&percent=" + this.state.form.percent +
                    "&command=" + encodeURIComponent(this.state.form.command);

        this._runAjax(query, "Command successfull", "Command failed", notify);
    }

    _resolveState() {
        let currentState = this.state ? this.state : {form: {pool: 1, percent: 100, command: ""}};
        return currentState;
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

        this._runAjax(query, "Environment has been updated", "Failed to save new environment", notify);
    }

};

BenchScenario.propTypes = {
    bench: React.PropTypes.object.isRequired
};

export default BenchScenario;
