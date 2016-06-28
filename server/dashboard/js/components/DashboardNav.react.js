import React from 'react';

class DashboardNav extends React.Component {
    render() {
        const tabs = {
            overview: "Overview",
            settings: "Settings"
        };

        return (
            <ul className="nav nav-tabs bench-nav">
                {Object.keys(tabs).map(function (tab) {
                    let name = tabs[tab];
                    let cssClass =  (this.props.selectedTab == tab) ? "active" : "";
                    let link = `#/dashboard/${this.props.item.id}/${tab}`;
                    return (<li role="presentation" key={tab} className={cssClass}><a href={link}>{name}</a></li>);
                }.bind(this))}
            </ul>
        );
    }
};

DashboardNav.propTypes = {
    item: React.PropTypes.object.isRequired,
    selectedTab: React.PropTypes.string
};

DashboardNav.defaultProps = {
    selectedTab: "overview"
};

export default DashboardNav;
