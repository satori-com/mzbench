import React from 'react';
import AceEditor from 'react-ace';
import BenchStore from '../stores/BenchStore';
import MZBenchRouter from '../utils/MZBenchRouter';
import MZBenchActions from '../actions/MZBenchActions';
import BenchChecker from '../utils/BenchChecker';
import AuthStore from '../stores/AuthStore';

require('brace/mode/python');
require('brace/theme/github');

class NewBench extends React.Component {
    constructor(props) {
            super(props);
            this._onChangeText = this._onChangeText.bind(this);
            this._onChangeName = this._onChangeName.bind(this);
            this._onCancel =     this._onCancel.bind(this);
            this._onChangeNodes = this._onChangeNodes.bind(this);
            this._onChangeCloud = this._onChangeCloud.bind(this);
            this._onChangeVarName = this._onChangeVarName.bind(this);
            this._onChangeVarValue = this._onChangeVarValue.bind(this);
            this._onAddVariable = this._onAddVariable.bind(this);
            this._onAddFromScript = this._onAddFromScript.bind(this);
            this._onRemoveUnused = this._onRemoveUnused.bind(this);
            this._onRemoveVariable = this._onRemoveVariable.bind(this);
            this._onStart = this._onStart.bind(this);
            this._onForceStart = this._onForceStart.bind(this);
    }
    renderEnv(env, idx) {
        let rowClass = env.unused ? "row unused" : "row";
        return  (<div className={rowClass} key={env.id}>
                  <div className="form-group col-md-3">
                    <input type="text" rel={env.id} defaultValue={env.name} onChange={this._onChangeVarName} className="form-control" placeholder="Variable name" />
                  </div>
                  <div className="form-group col-md-1 equals">
                    =
                  </div>
                  <div className="form-group col-md-6">
                    <div className="input-group">
                        <input type="text" rel={env.id} defaultValue={env.value} onChange={this._onChangeVarValue} className="form-control" placeholder="Value" />
                        <div className="input-group-btn">
                                <a role="button" className="btn btn-danger" href="#" rel={env.id} onClick={this._onRemoveVariable}><span className="glyphicon glyphicon-remove"></span></a>
                        </div>
                    </div>
                  </div>
                </div>);
    }
    renderError(err, idx) {
        let className = "alert alert-" + err.severity;
        return (
            <div className={className} key={idx} role="alert">
                {err.text}
            </div>
        );
    }
    _onAddVariable(event) {
        event.preventDefault();
        MZBenchActions.withNewBench((b) => {
                var max = b.env.reduce((a, x) => x.id > a ? x.id : a, 0);
                b.env.push({name:"", value:"", id: (max + 1)});
            });
    }
    _onRemoveVariable(event) {
        event.preventDefault();
        let idx = parseInt($(event.target).closest('a').attr("rel"));
        MZBenchActions.withNewBench((b) => {b.env = b.env.filter((x) => x.id !== idx);});
    }
    _onChangeVarName(event) {
        event.preventDefault();
        let idx = parseInt($(event.target).attr("rel"));
        MZBenchActions.withNewBench((b) => {b.env = b.env.map((x) => {if (idx === x.id) x.name = event.target.value; return x;})});
    }
    _onChangeVarValue(event) {
        let idx = parseInt($(event.target).attr("rel"));
        MZBenchActions.withNewBench((b) => {b.env = b.env.map((x) => {if (idx === x.id) x.value = event.target.value; return x;})});
    }
    _onRemoveUnused() {
        let analyzed = BenchChecker.analyze(this.props.bench);
        let unused = analyzed.env.reduce((a, x) => {
            if (x.unused) a[x.name] = true;
            return a;
        }, {});
        MZBenchActions.withNewBench((b) => {b.env = b.env.filter((x) => !unused[x.name])});
    }
    _onAddFromScript() {
        let analyzed = BenchChecker.analyze(this.props.bench);
        MZBenchActions.withNewBench((b) => {b.env = b.env.concat(analyzed.extra)});
    }
    addButton(analyzed) {
        if (analyzed.extra.length > 0)
            return <button type="button" className="btn btn-success" onClick={this._onAddFromScript}><span className="glyphicon glyphicon-plus"></span>Add from script</button>
        return null;
    }
    removeButton(analyzed) {
        if (analyzed.env.some((x) => x.unused))
            return <button type="button" className="btn btn-unused" onClick={this._onRemoveUnused}><span className="glyphicon glyphicon-minus"></span>Remove unused</button>
        return null;
    }
    render() {
        let clouds = this.props.clouds;
        let bench = this.props.bench;
        let analyzed = BenchChecker.analyze(this.props.bench);

        return (
            <div className="fluid-container">
                <div className="row">
                  <div className="form-group col-md-4">
                    <label>Name</label>
                    <input type="text" defaultValue={bench.name} onChange={this._onChangeName} className="form-control" placeholder="Type something" />
                  </div>
                  <div className="form-group col-md-3">
                    <label>Nodes</label>
                    <input type="text" defaultValue={bench.nodes} onChange={this._onChangeNodes} className="form-control" placeholder="1" />
                  </div>
                  <div className="form-group col-md-3">
                    <label>Cloud</label>
                    <select defaultValue={bench.cloud} onChange={this._onChangeCloud} className="form-control">
                        {clouds.map((cloudId) => { return <option key={cloudId} value={cloudId}>{cloudId}</option>}) }
                    </select>
                  </div>
                </div>
                <div className="row">
                    <div className="form-group col-md-10">
                        <label>Environmental variables</label>
                    </div>
                </div>
                {analyzed.env.map(this.renderEnv, this)}
                <div className="row">
                    <div className="form-group col-md-10 text-right">
                        {this.removeButton(analyzed)}
                        &nbsp;
                        {this.addButton(analyzed)}
                        &nbsp;
                        <button type="button" className="btn btn-success" onClick={this._onAddVariable}><span className="glyphicon glyphicon-plus"></span>Add variable</button>
                    </div>
                </div>
                <div className="row">
                    <div className="form-group col-md-12">
                        <AceEditor
                            mode="python"
                            theme="github"
                            width="100%"
                            maxLines={100000}
                            onChange={this._onChangeText}
                            value={bench.script_body}
                            editorProps={{$blockScrolling: true}}
                            name="BENCH_EDITOR" />
                    </div>
                </div>
                {bench.warnings ? bench.warnings.map(this.renderError, this) : null}
                {bench.errors ? bench.errors.map(this.renderError, this) : null}
                <div className="row">
                    <div className="form-group col-md-12 text-right">
                        {bench.warnings ? (<button type="button" className="btn btn-info" onClick={this._onForceStart}>Force run</button>): null}
                        &nbsp;
                        <button type="button" className="btn btn-success" onClick={this._onStart}>Run</button>
                        &nbsp;
                        <button type="button" className="btn btn-danger" onClick={this._onCancel}>Cancel</button>
                    </div>
                </div>
            </div>
        );
    }
    _onChangeText(newValue) {
        MZBenchActions.withNewBench((b) => {b.script_body = newValue});
    }
    _onChangeName(event) {
        MZBenchActions.withNewBench((b) => {b.name = event.target.value});
    }
    _onCancel(event) {
        MZBenchActions.resetNewBench();
        MZBenchRouter.navigate("#", {});
        MZBenchActions.selectBenchById(undefined);
    }
    _onChangeNodes(event) {
        MZBenchActions.withNewBench((b) => {b.nodes = event.target.value});
    }
    _onChangeCloud(event) {
        MZBenchActions.withNewBench((b) => {b.cloud = event.target.value});
    }

    _onStart(event) {
        event.preventDefault();
        let bench = BenchStore.getNew();
        let warnings = BenchChecker.get_errors(bench);

        if (warnings.length === 0) {
            this._onForceStart(event);
        } else {
            MZBenchActions.withNewBench((b) => {b.warnings = warnings});
        }
    }

    _onForceStart(event) {
        event.preventDefault();
        let notify = $.notify({message: `Checking the benchmark... `}, {type: 'info', delay: 0});
        let formData = new FormData();
        let bench = BenchStore.getNew();
        let blob = new Blob([bench.script_body], { type: "text/plain"});
        formData.append('bench', blob, bench.script_name);
        let params = {
            benchmark_name: bench.name,
            nodes: bench.nodes,
            cloud: bench.cloud};

        let start_query = MZBenchRouter.buildLink('/start', params);
        let typecheck_query = MZBenchRouter.buildLink('/typecheck', params);

        let env_query = bench.env.map((x, idx) =>
            x.name ? "&" + encodeURIComponent(x.name) + "=" + encodeURIComponent(x.value) : "").join("");

        $.ajax({url: typecheck_query + env_query, type : 'POST',
            processData: false,
            contentType: false,
            beforeSend: function (xhr) {
                if (AuthStore.getRef()) {
                    xhr.setRequestHeader("Authorization", "Bearer " + AuthStore.getRef() );
                }
            },
            data: formData,
            success: (checkdata) => {
                notify.update({message: `Starting...`, type: 'info', delay: 0});
                $.ajax({url: start_query + env_query, type : 'POST',
                    processData: false,
                    contentType: false,
                    data: formData,
                    success: (data) => {
                        notify.update({message: `Started benchmark`, type: 'success'});
                        MZBenchActions.resetNewBench();
                        MZBenchActions.selectBenchById(data.id);
                        MZBenchRouter.navigate("#/bench/" + data.id + "/overview", {});
                        setTimeout(() => notify.close(), 5000);
                    },
                    error: (data) => {
                        let msg = "Failed to start benchmark\n";
                        if (data.responseJSON && data.responseJSON.reason)
                            msg += `<br>${data.responseJSON.reason.split('\n')[0]}`;
                        notify.update({message: msg, type: 'danger'});
                        setTimeout(() => notify.close(), 5000);
                    }});
                },
            error: (data) => {
                let msg = "";
                if (data.responseJSON && data.responseJSON.reason)
                    msg = data.responseJSON.reason;
                else msg = "" + data;
                notify.update({message: 'Typecheck error', type: 'danger'});
                setTimeout(() => notify.close(), 5000);
                MZBenchActions.withNewBench((b) => {b.errors = [{severity:"danger", text:msg}]});
            }});
    }
};

NewBench.propTypes = {
    bench: React.PropTypes.object,
    clouds: React.PropTypes.array
};

export default NewBench;
