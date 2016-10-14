import React from 'react';
import ReactDOM from 'react-dom';
import AuthStore from '../stores/AuthStore';
import MZBenchWS from '../utils/MZBenchWS';
import Modal from './Modal.react';

class Auth extends React.Component {
    constructor(props) {
        super(props);
        this.state = this._resolveState();
        this.mounted = false;
        this._onChange = this._onChange.bind(this);
    }

    componentDidMount() {
        $(ReactDOM.findDOMNode(this.refs.authModal)).modal({backdrop: "static", show: true});
        this.state.authRequired ? this.open() : this.close();
        this.mounted = true;
        AuthStore.onChange(this._onChange);
    }

    componentWillUnmount() {
        AuthStore.off(this._onChange);
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

    ensureGAPILoaded(clientId) {
        if (this.gapi_loaded) return;
        this.gapi_loaded = true;

        ((d, s, id, cb) => {
          const element = d.getElementsByTagName(s)[0];
          const fjs = element;
          let js = element;
          js = d.createElement(s);
          js.id = id;
          js.src = '//apis.google.com/js/client:platform.js';
          fjs.parentNode.insertBefore(js, fjs);
          js.onload = cb;
        })(document, 'script', 'google-login', () => {
          const params = {
            client_id: clientId,
          };
          window.gapi.load('auth2', () => {
            if (!window.gapi.auth2.getAuthInstance()) {
                window.gapi.auth2.init(params);
            }
          });
        });
    }

    onGoogleSigninReq(event) {
        event.preventDefault();
        let auth2 = window.gapi.auth2.getAuthInstance();
        auth2.grantOfflineAccess({'redirect_uri': 'postmessage'}).then(
            (res) => {
                MZBenchWS.send({cmd: "one-time-code", type: "google", data: res.code});
            });
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
        return (
            <div>
                { (login != "") && (login != "anonymous") ?
                  <table className="signed-in-user-table"><tbody>
                    <tr><td>{this.state.userPic != "" ? <img src={this.state.userPic} height="38px"/> : null}</td>
                    <td className="signed-in-user-name">
                      <div>{this.state.userName != "" ? this.state.userName : this.state.userLogin}</div>
                      <div><a href="#" onClick={this.onSignOut.bind(this)}>Sign out</a></div>
                      <div><a href="#" onClick={this.onCreateToken.bind(this)}>Create token</a></div>
                    </td></tr>
                  </tbody></table> : null}
                <div ref="authModal" className="modal fade">
                    <div className="modal-dialog">
                        <div className="modal-content">
                            <div className="modal-header">
                                <h4 className="modal-title">{this.props.title}</h4>
                            </div>

                            <div className="modal-body">
                                <button type="button" className="btn btn-primary btn-lg btn-block" onClick={this.onGoogleSigninReq.bind(this)}>Google</button>
                            </div>
                        </div>
                    </div>
                </div>
                <Modal ref="createTokenModal" title="Generate a token for CLI utilites">
                    {this.state.generatedToken ?
                        <div className="auth-token-modal">
                            <p>Here is your secret token:</p> <pre>{this.state.generatedToken}</pre>
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
                </Modal>
                {login != "" ? this.props.children : null}
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

        if (support.google) {
            this.ensureGAPILoaded(support.google);
        }

        return {
            authRequired: needShowSignIn,
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
