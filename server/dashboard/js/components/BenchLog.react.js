import React from 'react';
import LogsStore from '../stores/LogsStore';
import MZBenchActions from '../actions/MZBenchActions';
import MZBenchRouter from '../utils/MZBenchRouter';

const LOGS_PER_PAGE = 500;

class BenchLog extends React.Component {
    constructor(props) {
        super(props);
        this.streamId = 0;
        this.autoSearchHandler = null;
        this.state = this._resolveState();
        this.state.tempQ = this.state.form.query;
        this.state.tempK = this.state.form.kind;
        this.state.tempE = this.state.form.errors;
        this.state.page = 0;
        this.endReached = false;
        this._onChange = this._onChange.bind(this);
        this._onChangeSearch = this._onChangeSearch.bind(this);
        this._onUser = this._onUser.bind(this);
        this._onSystem = this._onSystem.bind(this);
        this._onErrors = this._onErrors.bind(this);
        this._onScroll = this._onScroll.bind(this);
    }

    componentDidMount() {
        LogsStore.onChange(this._onChange);
        this.streamId = LogsStore.subscribeToLogs(this.props.bench.id);
        window.addEventListener("scroll", this._onScroll);
    }

    componentWillUnmount() {
        LogsStore.off(this._onChange);
        LogsStore.unsubscribeFromLogs(this.streamId);
        window.removeEventListener("scroll", this._onScroll);
    }

    render() {
        const url = '/' + (this.state.form.kind == 0 ? 'userlog' : 'log') + '?id=' + this.props.bench.id;
        let classUser = "btn btn-" + (this.state.form.kind == 0 ? "primary":"default");
        let classSystem = "btn btn-" + (this.state.form.kind == 1 ? "primary":"default");
        let classError = "btn btn-" + (this.state.form.errors == 1 ? "danger":"default");
        let currentLog = this.state.form.kind == 1 ? this.state.logs.system : this.state.logs.user;
        let logAfterQuery = null;
        let overflow = this.state.form.kind == 1 ? this.state.logs.systemOverflow : this.state.logs.userOverflow;

        logAfterQuery = this.filterLogs();

        this.endReached = (logAfterQuery.length <= (this.state.page + 1)*LOGS_PER_PAGE)

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
                <div className="zero-raw"><span className="btn-raw"><a href={url} target="_blank">Raw log</a></span></div>
                <div className="log-window">
                    <table className="table table-striped">
                        <tbody>
                        {overflow ? <tr className="warning"><td>Warning: due to big size, this log is trimmed</td></tr> : null}
                        {!currentLog.length ? <tr className="warning"><td>Warning: this log is empty</td></tr> : 
                            (!logAfterQuery.length ? <tr className="warning"><td>Query not found</td></tr> : null)}
                        {this.formatLogs(logAfterQuery)}
                        </tbody>
                    </table>
                </div>
            </div>
        );
    }

    filterLogs() {
        let currentLog = this.state.form.kind == 1 ? this.state.logs.system : this.state.logs.user;
        let query = this.state.form.query;
        let errors = this.state.form.errors;
        if (!query && !errors) return currentLog;

        var logAfterQuery = currentLog.filter((line) => {
            if (query) {
                let fullText = line.time + " " + line.severity + " " + line.text;
                if (fullText.indexOf(query) == -1) return false;
            }
            if (errors && line.severity!="[error]") return false;
            return true;
        });
        return logAfterQuery;
    }

    formatLogs(log) {
        let query = this.state.form.query;
        var res = [];
        for (var i = 0; i < (this.state.page + 1) * LOGS_PER_PAGE; i++) {
            let line = log[i];

            if (!line) break;

            let cssClass = line.severity == "[error]" ? "danger" : (line.severity == "[warning]" ? "warning": "");

            if (!query) {
                res.push(<tr key={line.id} className={cssClass}><td><pre>{line.time} {line.severity} {line.text}</pre></td></tr>);
            } else {
                let fullText = line.time + " " + line.severity + " " + line.text;
                let pieces = fullText.split(query);
                let idPieces = [];
                for(var j=1; j < pieces.length; j++)
                    idPieces.push({id: j, v: pieces[j]});
                res.push(<tr key={line.id} className={cssClass}><td><pre>{pieces[0]}{idPieces.map((f) => {return <span key={f.id}><mark>{query}</mark>{f.v}</span>})}</pre></td></tr>);
            }
        }
        return res;
    }

    _onScroll(scrollEvent) {
        let node = document.body;
        var shouldIncrementPage = (node.scrollTop + window.innerHeight + 2000 > node.scrollHeight);
        if (shouldIncrementPage && !this.endReached) {
            let state = this.state;
            state.page = state.page + 1;
            this.setState(state);
        }
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
        this.state.page = 0;
        this.endReached = false;
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
