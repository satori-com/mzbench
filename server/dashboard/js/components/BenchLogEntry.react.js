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
        let res = [];
        for (var i = from; i < to; i++) {
            let line = log[i];

            if (!line) break;

            let cssClass = line.severity == "[error]" ? "danger" : (line.severity == "[warning]" ? "warning": "");

            let fullText = line.time + " " + line.severity + line.text;

            if (!query) {
                res.push(
                    <tr key={line.id} className={cssClass}>
                        <td>
                            <pre>{fullText}</pre>
                        </td>
                    </tr>);
            } else {
                let pieces = fullText.split(query);

                let logLine = [pieces[0]];
                for (var k = 1; k < pieces.length; k++) {
                    logLine.push(<mark key={k}>{query}</mark>);
                    logLine.push(pieces[k])
                }
                res.push(
                    <tr key={line.id} className={cssClass}>
                        <td>
                            <pre>{logLine}</pre>
                        </td>
                    </tr>);
            }
        }
        this.nShown = i;
        return <table className="table table-striped table-logs"><tbody>{res}</tbody></table>;
    }
};

export default BenchLogEntry;
