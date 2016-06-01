import React from 'react';
import Modal from './Modal.react';

class GraphModal extends Modal {
    open() {
        super.open();
        
        this.oldWindowOnKeyDown = window.onkeydown;
        window.onkeydown = this._onKeyDown.bind(this);
    }

    close() {
        window.onkeydown = this.oldWindowOnKeyDown;
        this.oldWindowOnKeyDown = undefined;
        
        super.close();
    }
    
    render() {
        return (
            <div ref="modal" className="modal fade">
                <div className="modal-dialog fullscreen-modal-dialog">
                    <div className="modal-content">
                        <div className="modal-body">
                            {this.props.children}
                        </div>
                        
                        <div className="modal-footer">
                            <button type="button" className="btn btn-danger" onClick={this.props.onOk}>Close</button>
                        </div>
                    </div>
                </div>
            </div>
        );
    }
    
    _onKeyDown(event) {
        switch(event.keyCode) {
        case 27:                    // Escape
        case 13:                    // Enter
            event.preventDefault();
            this.props.onOk();
            break;
        }
    }
};

export default GraphModal;
