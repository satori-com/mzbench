import React from 'react';
import ReactDOM from 'react-dom';
import AuthStore from '../stores/AuthStore';
import MZBenchActions from '../actions/MZBenchActions';
import MZBenchWS from '../utils/MZBenchWS';
import Modal from './Modal.react';
import Menu, {SubMenu, MenuItem} from 'rc-menu';
import MZBenchRouter from '../utils/MZBenchRouter';
import LoadingSpinner from './LoadingSpinner.react';

class Auth extends React.Component {
    constructor(props) {
        super(props);
        this.state = this._resolveState();
        this.mounted = false;
        this._onChange = this._onChange.bind(this);
    }

    componentDidMount() {
        $(ReactDOM.findDOMNode(this.refs.authModal)).modal({backdrop: "static", show: true});
        this.mounted = true;
        AuthStore.onChange(this._onChange);
    }

    componentWillUnmount() {
        AuthStore.off(this._onChange);
        this.serverRequest.abort();
        MZBenchActions.unsubscribeBenchTimeline();
    }

    open() {
        $(ReactDOM.findDOMNode(this.refs.authModal)).modal("show");
    }

    close() {
        $(ReactDOM.findDOMNode(this.refs.authModal)).modal("hide");
    }

    _onChange() {
        this.setState(this._resolveState());
    }

    onSignOut() {
        let type = AuthStore.authType();

        if ("google2" == type) {
            let auth2 = window.gapi.auth2.getAuthInstance();
            auth2.signOut();
        }

        AuthStore.signOut();
    }

    onCreateToken() {
        $(ReactDOM.findDOMNode(this.refs.createTokenModal)).modal("show");
        this.setState({generatedToken: ""});
    }

    render() {
        let login = this.state.userLogin;
        let methods = this.state.supportedMethods;
        let modalWindow = (<div ref="authModal" className="modal fade">
                    <div className="modal-sign-in-dialog">
                        <div className="modal-content">
                            <div className="modal-header">
                                <h4 className="modal-title sign-in-header">{this.props.title}</h4>
                            </div>

                            <div className="modal-body">
                                {methods && methods.google ? <button type="button" className="btn btn-block btn-social btn-google" onClick={(event) => {event.preventDefault(); AuthStore.onGoogleSigninReq();}}>Google</button> : null}
                            </div>
                        </div>
                    </div>
                </div>)

        let tokenGenWindow =
                (<Modal ref="createTokenModal" title="Generate a token for CLI utilites">
                    {this.state.generatedToken ?
                        <div className="auth-token-modal">
                            <p>Your secret token:</p> <pre>{this.state.generatedToken}</pre>
                            <p>Please copy-paste it to ~/.config/mzbench/token file in your home directory</p>
                        </div> :
                        <div className="auth-token-modal">
                            How long do you want the token to be valid:
                            <select ref="validTime" defaultValue="86400">
                                <option value="3600">an hour</option>
                                <option value="86400">a day</option>
                                <option value="2592000">a month</option>
                                <option value="31536000">a year</option>
                                <option value="0">forever</option>
                            </select>&nbsp;
                            <button type="button" className="btn btn-primary btn-sm" onClick={this.onGenerateToken.bind(this)}>Generate</button>
                        </div>
                    }
                </Modal>)
        return (
            <div>
                { (login != "") && (login != "anonymous") ?
                  <table className="signed-in-user-table"><tbody>
                    <tr><td>{this.state.userPic != "" ? <img src={this.state.userPic} height="38px"/> : null}</td>
                    <td>
                        <Menu mode='horizontal' openAnimation='slide-up'>
                           <SubMenu title={this.state.userName != "" ? this.state.userName : this.state.userLogin}>
                            <MenuItem>
                                <div><a href="#" onClick={this.onCreateToken.bind(this)}>Generate token</a></div>
                            </MenuItem>
                            <MenuItem>
                                <div><a href="#" onClick={this.onSignOut.bind(this)}>Sign out</a></div>
                            </MenuItem>
                          </SubMenu>
                        </Menu>
                    </td></tr>
                  </tbody></table> : null}
                {modalWindow}
                {tokenGenWindow}
                {methods == null && login == "" ? <LoadingSpinner>Loading...</LoadingSpinner> :
                    (login != "" ? this.props.children : null)}
            </div>
        );
    }

    onGenerateToken() {
        let tokenLifetime = this.refs.validTime.value;
        AuthStore.requestToken(tokenLifetime);
    }

    _resolveState() {
        let needShowSignIn = AuthStore.isAuthRequired();
        if (this.mounted) {
            needShowSignIn ? this.open() : this.close();
        }
        let support = AuthStore.supportedAuthMethods();


        return {
            supportedMethods: support,
            userLogin: AuthStore.userLogin(),
            userName: AuthStore.userName(),
            userPic: AuthStore.userPic(),
            generatedToken: AuthStore.getGeneratedToken()
        };
    }
};

Auth.propTypes = {
    title: React.PropTypes.string,
    onOk: React.PropTypes.func
};

Auth.defaultProps = {
    title: ""
};

export default Auth;
