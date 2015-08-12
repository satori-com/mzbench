import React, { PropTypes } from 'react';
import BenchTabs from '../constants/BenchTabs';

class BenchNav extends React.Component {
    render() {
        const tabs = {
            overview: "Overview",
            scenario: "Scenario",
            metrics:  "Metrics",
            logs:     "Logs"
        };

        return (
            <ul className="nav nav-tabs bench-nav">
                {Object.keys(tabs).map(function (tab) {
                    let name = tabs[tab];
                    let cssClass =  (this.props.selectedTab == tab) ? "active" : "";
                    let link = `#/bench/${this.props.bench.id}/${tab}`;
                    return (<li role="presentation" key={tab} className={cssClass}><a href={link}>{name}</a></li>);
                }.bind(this))}
            </ul>
        );
    }
}

BenchNav.propTypes = {
    bench: React.PropTypes.object.isRequired,
    selectedTab: React.PropTypes.string
};

BenchNav.defaultProps = {
    selectedTab: "overview"
};

export default BenchNav;
