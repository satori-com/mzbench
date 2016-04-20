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
        this.state.logShown = LOGS_PER_PAGE;
        this.isFollow = false;
        this.followFlag = false;
        this._onChange = this._onChange.bind(this);
        this._onChangeSearch = this._onChangeSearch.bind(this);
        this._onUser = this._onUser.bind(this);
        this._onSystem = this._onSystem.bind(this);
        this._onErrors = this._onErrors.bind(this);
        this._onScroll = this._onScroll.bind(this);
        this._onFollow = this._onFollow.bind(this);
        this._onTop = this._onTop.bind(this);
        this.filtered = 0;
        this.lastLogShown = 0;
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

    componentDidUpdate() {
        if (this.isFollow) {
            this.goBottom();
        }
    }

    shouldComponentUpdate(nextProps, nextState) {
        let isNewLogs = (this.lastLogShown != nextState.logShown);
        let isBenchComplete = (this.props.isBenchActive != nextProps.isBenchActive);
        return isNewLogs || isBenchComplete;
    }

    render() {
        const url = '/' + (this.state.form.kind == 0 ? 'userlog' : 'log') + '?id=' + this.props.bench.id;
        let classUser = "btn btn-" + (this.state.form.kind == 0 ? "primary":"default");
        let classSystem = "btn btn-" + (this.state.form.kind == 1 ? "primary":"default");
        let classError = "btn btn-" + (this.state.form.errors == 1 ? "danger":"default");
        let currentLog = this.state.form.kind == 1 ? this.state.logs.system : this.state.logs.user;
        let overflow = this.state.form.kind == 1 ? this.state.logs.systemOverflow : this.state.logs.userOverflow;

        let logAfterQuery = this.state.logAfterQuery;

        this.lastLogShown = (logAfterQuery.length < this.state.logShown) ? logAfterQuery.length : this.state.logShown;

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
                <div className="zero-raw">
                    <span className="btn-raw">
                        <a href={url} target="_blank">Raw log</a>
                        <a href="#" onClick={this._onFollow}>{this.props.isBenchActive ? "Follow" : "Bottom"}</a>
                        <a href="#" onClick={this._onTop}>Top</a>
                    </span>
                </div>
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

    filterLogs(form, logs, needRefilter) {
        let filtered = needRefilter ? 0 : this.filtered;
        var logAfterQuery = (needRefilter || !this.state) ? [] : this.state.logAfterQuery;
        let currentLog = form.kind == 1 ? logs.system : logs.user;
        let query = form.query;
        let errors = form.errors;
        if (needRefilter) this.lastLogShown = 0;
        if (!query && !errors) {
            this.filtered = currentLog.length;
            return currentLog;
        }

        for (; filtered < currentLog.length; filtered++) {
            let line = currentLog[filtered];
            if (!line) break;
            if (query) {
                let fullText = line.time + " " + line.severity + " " + line.text;
                if (fullText.indexOf(query) == -1) continue;
            }
            if (errors && line.severity!="[error]") continue;
            logAfterQuery.push(line);
        };
        this.filtered = filtered;
        return logAfterQuery;
    }

    formatLogs(log) {
        let query = this.state.form.query;
        var res = [];
        for (var i = 0; i < this.state.logShown; i++) {
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
        if (this.updatePage()) return;

        if (this.followFlag) {
            this.followFlag = false;
        } else if(this.isFollow) {
            this.isFollow = false;
        }
    }

    _resolveState() {
        let currentState = this.state ? this.state : {form: {query: "", kind: 0, errors: 0}, logAfterQuery: []};
        let newForm = LogsStore.getQueryData(this.props.bench.id);
        let needRefilter = (!currentState.form ||
                            currentState.form.query  != newForm.query ||
                            currentState.form.kind   != newForm.kind  ||
                            currentState.form.errors != newForm.errors);
        currentState.form.kind = newForm.kind;
        currentState.form.query = newForm.query;
        currentState.form.errors = newForm.errors;
        currentState.logs = LogsStore.getLogData(this.streamId);
        currentState.logAfterQuery = this.filterLogs(newForm, currentState.logs, needRefilter);
        return currentState;
    }

    _onChange() {
        this.setState(this._resolveState());
        if (this.isFollow) this.updatePage();
    }

    _runSearch() {
        this.state.logShown = LOGS_PER_PAGE;
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

    _onFollow(event) {
        event.preventDefault();
        this.isFollow = !this.isFollow;
        let logLen = this.state.logAfterQuery.length;
        if (this.state.logShown < logLen) {
            this.setState({logShown: logLen});
        }
        if (this.isFollow) this.goBottom();
    }

    _onTop(event) {
        event.preventDefault();
        this.isFollow = false;
        this.goTop();
    }

    _onErrors() {
        this.state.tempE = !this.state.tempE;
        this._runSearch();
    }

    goBottom() {
        var newValue = document.body.scrollHeight - window.innerHeight;
        if (document.body.scrollTop < newValue) {
            document.body.scrollTop = newValue;
            this.followFlag = true;
        }
    }

    goTop() {
        document.body.scrollTop = 0;
    }

    updatePage () {
        let node = document.body;
        var shouldIncrementPage = (node.scrollTop + window.innerHeight + 2000 > node.scrollHeight);
        let endReached = (this.state.logAfterQuery.length <= this.state.logShown);
        if (shouldIncrementPage && !endReached) {
            this.setState({logShown: this.state.logShown + LOGS_PER_PAGE});
            return true;
        }
        return false;
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
