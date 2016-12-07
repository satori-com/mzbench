import moment from 'moment';
import { EventEmitter } from 'events';
import Dispatcher from '../dispatcher/AppDispatcher';
import ActionTypes from '../constants/ActionTypes';
import MZBenchWS from '../utils/MZBenchWS';
import MZBenchRouter from '../utils/MZBenchRouter';
import MZBenchActions from '../actions/MZBenchActions';

const CHANGE_EVENT = 'auth_change';

class AuthStore extends EventEmitter {
    constructor() {
        super();
        this.reauth();
    }

    reauth() {
        this.resetUserData();
        this.serverRequest = $.ajax({
                  type: 'POST',
                  url: MZBenchRouter.buildLink('/auth', {type: "ref"}),
                  contentType: 'application/octet-stream; charset=utf-8',
                  success: this.handleSuccAuth.bind(this),
                  processData: false
                });
    }

    handleSuccAuth(response) {
        if (response.res == "ok") {
            MZBenchActions.subscribeBenchTimeline();
            this.login = response.user_info.login;
            this.name = response.user_info.name;
            this.picture = response.user_info.picture_url;
            this.emit(CHANGE_EVENT);
        } else if (response.res == "error"){
            this.handleAuthMethodsResponse(response.use);
        }
    }

    handleAuthMethodsResponse(support) {

        if (support.google) {
            this.ensureGAPILoaded(support.google);
        }

        if (Object.keys(support).length > 0) {
            this.authMethods = support;

            this.emit(CHANGE_EVENT);
        } else {
            this.authMethods = support;
            this.login = "anonymous";
            this.emit(CHANGE_EVENT);
        }
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

    onGoogleSigninReq() {
        let auth2 = window.gapi.auth2.getAuthInstance();
        auth2.grantOfflineAccess({'redirect_uri': 'postmessage'}).then(
            (res) => {
                $.ajax({
                  type: 'POST',
                  url: MZBenchRouter.buildLink('/auth', {type: "google"}),
                  contentType: 'application/octet-stream; charset=utf-8',
                  success: this.handleSuccAuth.bind(this),
                  processData: false,
                  data: res.code
                });
            });
    }

    emitChange() {
        return this.emit(CHANGE_EVENT);
    }

    onChange(callback) {
        this.on(CHANGE_EVENT, callback);
    }

    off(callback) {
        this.removeListener(CHANGE_EVENT, callback);
    }

    isAuthRequired() {
        return (this.login == "");
    }

    supportedAuthMethods() {
        return this.authMethods;
    }

    isAnonymousServer() {
        return this.authMethods.length == 0;
    }

    userLogin() {
        return this.login;
    }

    userName() {
        return this.name;
    }

    userPic() {
        return this.picture;
    }

    authType() {
        return this.type;
    }

    getRef() {
        return this.ref;
    }

    handleTokenExpired() {
        console.log("Token expired");
        MZBenchActions.unsubscribeBenchTimeline();
        this.reauth();
    }

    signOut() {
        $.ajax({
          type: 'POST',
          url: MZBenchRouter.buildLink('/sign-out', {}),
          contentType: 'application/octet-stream; charset=utf-8',
          processData: false,
          success: () => {
                MZBenchActions.unsubscribeBenchTimeline();
                this.reauth();
            }
        });
    }

    requestToken(tokenLifetime) {
        MZBenchWS.send({cmd: "generate-token", lifetime: tokenLifetime.toString()});
    }

    handleGeneratedToken(token) {
        this.generatedToken = token;
    }

    getGeneratedToken() {
        return this.generatedToken;
    }

    resetUserData() {
        this.login = "";
        this.picture = "";
        this.name = "";
        this.generatedToken = "";
        this.authMethods = [];
    }
}


var _AuthStore = new AuthStore();
export default _AuthStore;

_AuthStore.dispatchToken = Dispatcher.register((action) => {
    switch (action.type) {
        case ActionTypes.AUTH_TOKEN_EXPIRED:
            _AuthStore.handleTokenExpired();
            _AuthStore.emitChange();
            break;

        case ActionTypes.GENERATED_TOKEN:
            _AuthStore.handleGeneratedToken(action.token);
            _AuthStore.emitChange();
            break;

        default:
    }
});

