import React from 'react';

class LoadingSpinner extends React.Component {
    render() {
        return (
            <div className="alert alert-info" role="alert">
                <span className="glyphicon glyphicon-refresh glyphicon-spin"></span>&nbsp;
                {this.props.children}
            </div>
        );
    }
};

export default LoadingSpinner;
