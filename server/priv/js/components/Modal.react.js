import React, { findDOMNode } from 'react';

class Modal extends React.Component {
    componentDidMount() {
        $(findDOMNode(this.refs.modal)).modal({backdrop: "static", keyboard: true, show: false});
    }

    componentWillUnmount() {
        $(findDOMNode(this.refs.modal)).off("hidden", this.handleHidden);
    }

    open() {
        $(findDOMNode(this.refs.modal)).modal("show");
    }

    close() {
        $(findDOMNode(this.refs.modal)).modal("hide");
    }

    renderHeader() {
        if(this.props.render_title) {
            return (
                <div className="modal-header">
                    <button type="button" className="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
                    <h4 className="modal-title">{this.props.title}</h4>
                </div>
            );
        } else {
            return;
        }
    }

    renderFooter() {
        if(this.props.render_submit_button) {
            return (
                <div className="modal-footer">
                    <button type="button" className="btn btn-default" data-dismiss="modal">Close</button>
                    <button type="button" className="btn btn-primary" onClick={this.props.onOk}>Submit</button>
                </div>
            );
        } else {
            return (
                <div className="modal-footer">
                    <button type="button" className="btn btn-default" onClick={this.props.onOk}>Close</button>
                </div>
            );
        }
    }

    render() {
        return (
            <div ref="modal" className="modal fade">
                <div className={this.props.render_fullscreen?"modal-dialog fullscreen-modal-dialog":"modal-dialog"}>
                    <div className="modal-content">
                        {this.renderHeader()}
                        <div className="modal-body">
                            {this.props.children}
                        </div>
                        {this.renderFooter()}
                    </div>
                </div>
            </div>
        );
    }
};

Modal.propTypes = {
    title: React.PropTypes.string,
    render_fullscreen: React.PropTypes.bool,
    render_title: React.PropTypes.bool,
    render_submit_button: React.PropTypes.bool
};

Modal.defaultProps = {
    title: "",
    render_fullscreen: false,
    render_title: true,
    render_submit_button: true
};

export default Modal;
