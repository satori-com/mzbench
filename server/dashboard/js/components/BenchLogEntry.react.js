import React from 'react';

class BenchLogEntry extends React.Component {
    constructor(props) {
        super(props);
        this.state = {};
        this.nShown = props.from;
    }

    shouldComponentUpdate(nextProps, nextState) {
        return (nextProps.log.length < nextProps.to) || this.nShown < nextProps.to;
    }

    render() {
        let query = this.props.query;
        let from = this.props.from;
        let to = this.props.to;
        let log = this.props.log;
        var res = [];
        for (var i = from; i < to; i++) {
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
        this.nShown = i;
        return <table className="table table-striped table-logs"><tbody>{res}</tbody></table>;
    }
};

export default BenchLogEntry;
