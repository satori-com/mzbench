import React from 'react';
import ReactDOM from 'react-dom';
import PropTypes from 'prop-types';

class Modal extends React.Component {
    componentDidMount() {
        $(ReactDOM.findDOMNode(this.refs.modal)).modal({backdrop: "static", show: false});
    }

    componentWillUnmount() {
        $(ReactDOM.findDOMNode(this.refs.modal)).off("hidden", this.handleHidden);
    }

    open() {
        $(ReactDOM.findDOMNode(this.refs.modal)).modal("show");
    }

    close() {
        $(ReactDOM.findDOMNode(this.refs.modal)).modal("hide");
    }

    render() {
        return (
            <div ref="modal" className="modal fade">
                <div className="modal-dialog">
                    <div className="modal-content">
                        <div className="modal-header">
                            <button type="button" className="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
                            <h4 className="modal-title">{this.props.title}</h4>
                        </div>
                        
                        <div className="modal-body">
                            {this.props.children}
                        </div>
                        
                        <div className="modal-footer">
                            <button type="button" className="btn btn-default" data-dismiss="modal">Close</button>
                            {this.props.onOk ? <button type="button" className="btn btn-primary" onClick={this.props.onOk}>Submit</button> : null}
                        </div>
                    </div>
                </div>
            </div>
        );
    }
};

Modal.propTypes = {
    title: PropTypes.string,
    onOk: PropTypes.func
};

Modal.defaultProps = {
    title: ""
};

export default Modal;
