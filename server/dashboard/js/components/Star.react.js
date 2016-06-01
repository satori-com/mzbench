import React from 'react';

class Star extends React.Component {
    constructor(props) {
        super(props);

        this.state = { hover: false, selected: props.selected};
    }

    componentWillReceiveProps(newProps) {
        if (newProps.selected != this.state.selected) {
            this.setState({selected: newProps.selected});
        }
    }

    render() {
        var starClass = "";

        if (this.state.selected) starClass = "favorite-star-character-active";
        else if (this.state.hover) starClass = "favorite-star-character-hover";

        return (
            <span className={"favorite-star-character " + starClass}
                onClick = {this._onClick.bind(this)}
                onMouseOver = {this._onMouseover.bind(this)}
                onMouseOut = {this._onMouseout.bind(this)}>
                &#x2605;
            </span>
        );
    }

    _onClick(event) {
        event.preventDefault();
        var newValue = !this.state.selected;
        if (this.props.onClick) this.props.onClick(newValue);
        this.setState({selected: newValue});
    }
    _onMouseover(event) {
        this.setState({hover: true});
    }
    _onMouseout(event) {
        this.setState({hover: false});
    }
};



export default Star;
