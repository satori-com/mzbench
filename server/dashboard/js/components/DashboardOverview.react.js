import React from 'react';
import DashboardStore from '../stores/DashboardStore';
import BenchStore from '../stores/BenchStore';
import Graph from './Graph.react';
import MZBenchActions from '../actions/MZBenchActions';
import Misc from '../utils/Misc';
import Collapsible from 'react-collapsible';
import moment from 'moment';


class DashboardOverview extends React.Component {
    constructor(props) {
        super(props);
        this.state = {};
        this.benchsetId = Misc.gen_guid();
    }

    componentDidMount() {
        MZBenchActions.subscribeBenchset({criteria:this.props.item.criteria,
            charts: this.props.item.charts, benchset_id: this.benchsetId});
    }

    componentWillUnmount() {
        MZBenchActions.unsubscribeBenchset(this.benchsetId);
    }

    render() {
        if (this.props.benchsetId != this.benchsetId) {
            return (<h3>Loading...</h3>);
        } else {
            return (
            <div className="panel panel-default graph-panel">
                <div className="panel-heading">
                    <h3 className="panel-title">
                        <span className="glyphicon glyphicon-collapse-down" />&nbsp;
                        <span className="graph-group-title">Charts for search string &quot;{this.props.item.criteria}&quot;</span>
                    </h3>
                </div>
                {this.renderGraphs()}
            </div>);
        }
    }

    renderLegend(benches) {
        return benches.map((b, idx1) => {
            return b.benches.map((bb, idx2) => {
                let idx = idx1+"_"+idx2;
                let time = moment(bb.time).add(BenchStore.getServerDateDiff()).format("lll");
                let link = "#/bench/" + bb.id + "/overview";
                return (<tr key={idx}>
                            <td></td>
                            <td><a href={link}>{b.name}</a></td>
                            <td><a href={link}>{bb.id}</a></td>
                            <td><a href={link}>{time}</a></td>
                            {bb.x ? (<td><a href={link}>{bb.x}</a></td>) : null}
                            {bb.final ? (<td><a href={link}>{+bb.final.toFixed(2)}</a></td>) : null}
                        </tr>);
            });
        });
    }

    renderTable(name, kind, groupEnv, xEnv, benches) {
        if (kind == "compare")
            return (<table className="table table-striped">
                <thead><tr><th></th><th>{groupEnv}</th><th>Bench Id</th><th>Time</th></tr></thead>
                <tbody>{this.renderLegend(benches)}</tbody>
            </table>);
        if (kind == "group")
            return (<table className="table table-striped">
                <thead><tr><th></th><th>{groupEnv}</th><th>Bench Id</th><th>Time</th><th>{xEnv}</th><th>{name}</th></tr></thead>
                <tbody>{this.renderLegend(benches)}</tbody>
            </table>);
        if (kind == "regression")
            return (<table className="table table-striped">
                <thead><tr><th></th><th>{groupEnv}</th><th>Bench Id</th><th>Time</th><th>{name}</th></tr></thead>
                <tbody>{this.renderLegend(benches)}</tbody>
            </table>);
    }

    renderGraphs() {
        return this.props.item.charts.map((c, idx) => {
            if (c.metric == "") return null;

            let benches = this.props.benchset[idx];
            let targets = [c.metric];

            if (!benches || benches.length == 0) return (<h4 key={idx}>{c.metric} not found</h4>);

            let guid = "compare-" + idx;
            let groupEnv = c.group_env;
            let xEnv = c.kind === "regression" ? (c.regression_x ? c.regression_x : "Number") : c.x_env;
            let key = "" + idx + benches.map((b) => b.benches.map((bb) => bb.id)).join("");

            return (<div key={key}>
                        <p className="dashboard">{c.description}</p>
                        <Graph targets={targets} kind={c.kind} x_env={xEnv}
                            title={c.metric} benchset={benches} domPrefix={guid} height="400"/>
                        <Collapsible triggerText="Show benches" triggerTextWhenOpen="Hide benches">
                            {this.renderTable(c.metric, c.kind, Misc.ucfirst(groupEnv), Misc.ucfirst(xEnv), benches)}
                        </Collapsible>
                    </div>);
        });
    }

};

DashboardOverview.propTypes = {
    item: React.PropTypes.object.isRequired
};

export default DashboardOverview;
