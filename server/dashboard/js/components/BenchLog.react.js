import React from 'react';
import LogsStore from '../stores/LogsStore';
import MZBenchActions from '../actions/MZBenchActions';
import MZBenchRouter from '../utils/MZBenchRouter';

class BenchLog extends React.Component {
    constructor(props) {
        super(props);
        this.streamId = 0;
        this.autoSearchHandler = null;
        this.state = this._resolveState();
        this.state.tempQ = this.state.form.query;
        this.state.tempK = this.state.form.kind;
        this.state.tempE = this.state.form.errors;
        this._onChange = this._onChange.bind(this);
        this._onChangeSearch = this._onChangeSearch.bind(this);
        this._onUser = this._onUser.bind(this);
        this._onSystem = this._onSystem.bind(this);
        this._onErrors = this._onErrors.bind(this);
    }

    componentDidMount() {
        LogsStore.onChange(this._onChange);
        this.streamId = LogsStore.subscribeToLogs(this.props.bench.id);
    }
    
    componentWillUnmount() {
        LogsStore.off(this._onChange);
        LogsStore.unsubscribeFromLogs(this.streamId);
    }

    render() {
        const url = '/logs' + (this.state.form.kind == 0 ? '_user' : '') + '?id=' + this.props.bench.id;
        let classUser = "btn btn-" + (this.state.form.kind == 0 ? "primary":"default");
        let classSystem = "btn btn-" + (this.state.form.kind == 1 ? "primary":"default");
        let classError = "btn btn-" + (this.state.form.errors == 1 ? "danger":"default");
        let currentLog = this.state.form.kind == 1 ? this.state.logs.system : this.state.logs.user;
        let overflow = this.state.form.kind == 1 ? this.state.logs.systemOverflow : this.state.logs.userOverflow;

        return (
            <div>
                <form className="form-inline log-lookup-form">
                    <div className="input-group col-xs-4">
                      <div className="input-group-addon"><span className="glyphicon glyphicon-search" aria-hidden="true"></span></div>
                      <input type="text" className="form-control" onKeyDown={this._onKeyDown.bind(this)} onChange={this._onChangeSearch} value={this.state.tempQ} placeholder="Lookup Logs" />
                    </div>
                  <div className="btn-group" role="group">
                    <button type="button" className={classUser} onClick={this._onUser}>User</button>
                    <button type="button" className={classSystem} onClick={this._onSystem}>System</button>
                  </div>
                  <button type="button" className={classError} onClick={this._onErrors}>Errors only</button>
                </form>
                <div className="zero-raw"><span className="btn-raw"><a href={url} target="_blank">Open raw</a></span></div>
                <div className="log-window">
                    <table className="table table-striped">
                        <tbody>
                        {overflow ? <tr className="warning"><td>Warning: due to big size, this log is trimmed</td></tr> : null}
                        {currentLog.map((line) => { 
                            let cssClass = line.severity == "[error]" ? "danger" : "";
                            let query = this.state.form.query;

                            if (this.state.form.errors && line.severity!="[error]") return null;

                            if (!query) {
                                return <tr key={line.id} className={cssClass}><td>{line.time} {line.severity} {line.text}</td></tr>
                            } else {
                                let fullText = line.time + " " + line.severity + " " + line.text;
                                if (fullText.indexOf(query)!= -1) {
                                    let pieces = fullText.split(query);
                                    let idPieces = [];
                                    for(var i=1; i < pieces.length; i++)
                                        idPieces.push({id: i, v: pieces[i]});
                                    return <tr key={line.id} className={cssClass}><td>{pieces[0]}{idPieces.map((f) => {return <span key={f.id}><mark>{query}</mark>{f.v}</span>})}</td></tr>
                                } else
                                    return null;
                            }})
                        }
                        </tbody>
                    </table>
                </div>
            </div>
        );
    }

    _resolveState() {
        let currentState = this.state ? this.state : {};
        currentState.form = LogsStore.getQueryData(this.props.bench.id);
        currentState.logs = LogsStore.getLogData(this.streamId);
        return currentState;
    }

    _onChange() {
        this.setState(this._resolveState());
    }

    _runSearch() {
        if (this.autoSearchHandler) {
            clearTimeout(this.autoSearchHandler);
        }
        MZBenchRouter.navigate("/bench/" + this.props.bench.id + "/logs/" +
            (this.state.tempK ? "system": "user") + "/" +
            (this.state.tempE ? "errors": "all") + (this.state.tempQ ? "/" + encodeURIComponent(this.state.tempQ) : ""), {});
    }

    _onKeyDown(event) {
        if (event.key === 'Enter') {
            event.preventDefault();
            this._runSearch();
        }
    }

    _onChangeSearch(event) {
        let state = this.state;
        if (this.autoSearchHandler) {
            clearTimeout(this.autoSearchHandler);
        }
        state.tempQ = event.target.value;
        this.setState(state);
        this.autoSearchHandler = setTimeout(() => this._runSearch(), this.props.autoSearchInterval);
    }

    _onUser() {
        this.state.tempK = 0;
        this._runSearch();
    }

    _onSystem() {
        this.state.tempK = 1;
        this._runSearch();
    }

    _onErrors() {
        this.state.tempE = !this.state.tempE;
        this._runSearch();
    }
};

BenchLog.propTypes = {
    bench: React.PropTypes.object.isRequired,
    autoSearchInterval: React.PropTypes.number
};

BenchLog.defaultProps = {
    autoSearchInterval: 500
};

export default BenchLog;
