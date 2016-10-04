import React from 'react';
import ReactDOM from 'react-dom';
import AuthStore from '../stores/AuthStore';
import MZBenchWS from '../utils/MZBenchWS';

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

    render() {
        return (
            <div>
                {this.state.userLogin ?
                  <table className="signed-in-user-table"><tbody>
                    <tr><td>{this.state.userPic ? <img src={this.state.userPic} height="38px"/> : null}</td>
                    <td className="signed-in-user-name">
                      <div>{this.state.userName ? this.state.userName : null}</div>
                      <div><a href="#" onClick={this.onSignOut.bind(this)}>Sign out</a></div>
                    </td></tr>
                  </tbody></table> : null}
                <div ref="authModal" className="modal fade">
                    <div className="modal-dialog">
                        <div className="modal-content">
                            <div className="modal-header">
                                <h4 className="modal-title">{this.props.title}</h4>
                            </div>

                            <div className="modal-body">
                                <a href="#" onClick={this.onGoogleSigninReq.bind(this)}>Sign-in with Google</a>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        );
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
            userPic: AuthStore.userPic()
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
