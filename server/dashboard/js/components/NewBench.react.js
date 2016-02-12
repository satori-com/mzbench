import React from 'react';
import AceEditor from 'react-ace';
import BenchStore from '../stores/BenchStore';
import MZBenchRouter from '../utils/MZBenchRouter';
import MZBenchActions from '../actions/MZBenchActions';

require('brace/mode/erlang');
require('brace/theme/github');

class NewBench extends React.Component {
    constructor(props) {
            super(props);
            this._onChangeText = this._onChangeText.bind(this);
            this._onChangeName = this._onChangeName.bind(this);
            this._onCancel =     this._onCancel.bind(this);
            this._onChangeNodes = this._onChangeNodes.bind(this);
            this._onChangeCloud = this._onChangeCloud.bind(this);
            this._onChangeEnv = this._onChangeEnv.bind(this);
            this._onStart = this._onStart.bind(this);
    }
    render() {
        let clouds = this.props.clouds;
        let bench = this.props.bench;
        let envStr = Object.keys(bench.env).map(
            (key) => {
                return key.toString() + "=" + bench.env[key].toString()
            }).join(", ");

        return (
            <div className="fluid-container">
                <div className="row">
                  <div className="form-group col-md-3">
                    <label>Name</label>
                    <input type="text" defaultValue={bench.benchmark_name} onChange={this._onChangeName} className="form-control" placeholder="Type something" />
                  </div>
                  <div className="form-group col-md-3">
                    <label>Nodes</label>
                    <input type="text" defaultValue={bench.nodes} onChange={this._onChangeNodes} className="form-control" placeholder="1" />
                  </div>
                  <div className="form-group col-md-3">
                    <label>Cloud</label>
                    <select defaultValue={bench.cloud} onChange={this._onChangeCloud} className="form-control">
                        {clouds.map((cloudId) => { return <option key={cloudId}>{cloudId}</option>}) }
                    </select>
                  </div>
                </div>
                <div className="row">
                    <div className="form-group col-md-12">
                        <label>Environmental variables (name1=value1[,;\n]name2=value2)</label>
                        <textarea className="form-control" onChange={this._onChangeEnv} rows="2" defaultValue={envStr}></textarea>
                    </div>
                </div>
                <div className="row">
                    <div className="form-group col-md-12">
                        <AceEditor
                            mode="erlang"
                            theme="github"
                            width="100%"
                            maxLines={100000}
                            onChange={this._onChangeText}
                            value={bench.script_body}
                            editorProps={{$blockScrolling: true}}
                            name="BENCH_EDITOR" />
                    </div>
                </div>
                <div className="row">
                    <div className="form-group col-md-12 text-right">
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
        MZBenchActions.withNewBench((b) => {b.benchmark_name = event.target.value});
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
    _onChangeEnv(event) {
        MZBenchActions.withNewBench((b) => {
            b.env = {};
            event.target.value.split(/[\n,;]+/).reduce((acc, x) => {
                var d = x.split("=");
                if (d.length >= 2) {
                    var key = d[0].trim();
                    var value = d[1].trim();
                    acc.env[key] = value;
                }
                return acc;
            }, b)
        });
    }

    _onStart(event) {
        event.preventDefault();
        let notify = $.notify({message: `Starting the benchmark... `}, {type: 'info', delay: 0});
        let formData = new FormData();
        let bench = BenchStore.getNewBench();
        let blob = new Blob([bench.script_body], { type: "text/plain"});
        formData.append('bench', blob, bench.script_name);
        let query = MZBenchRouter.buildLink('/start', {
            benchmark_name: bench.benchmark_name,
            nodes: bench.nodes,
            cloud: bench.cloud});

        query += Object.keys(bench.env).map((x) =>
            x ? "&" + encodeURIComponent(x) + "=" + encodeURIComponent(bench.env[x]) : "").join("");

        $.ajax({url: query, type : 'POST',
            processData: false,
            contentType: false,
            data: formData,
            success: (data) => {
                notify.update({message: `Benchmark has been started`, type: 'success'});
                MZBenchActions.resetNewBench();
                MZBenchActions.selectBenchById(data.id);
                MZBenchRouter.navigate("#/bench/" + data.id + "/overview", {});
                setTimeout(() => notify.close(), 5000);
            },
            error: () => {
                notify.update({message: `Failed to start benchmark`, type: 'danger'});
                setTimeout(() => notify.close(), 5000);
            }});
    }
};

NewBench.propTypes = {
    bench: React.PropTypes.object,
    clouds: React.PropTypes.array
};

export default NewBench;
