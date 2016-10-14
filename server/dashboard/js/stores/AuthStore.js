import moment from 'moment';
import { EventEmitter } from 'events';
import Dispatcher from '../dispatcher/AppDispatcher';
import ActionTypes from '../constants/ActionTypes';
import MZBenchWS from '../utils/MZBenchWS';

const CHANGE_EVENT = 'auth_change';

class AuthStore extends EventEmitter {
    constructor() {
        super();
        this.resetUserData();
        this.authMethods = {};
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
        return (this.ref == "");
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

    handleAuthReq(support) {
        if (this.ref == "") {
            this.resetUserData();
            this.authMethods = support;
        } else {
            MZBenchWS.send({cmd: "my-ref", data: this.ref});
        }

    }

    handleAuthOk(login, type, name, picture, ref) {
        this.type = type;
        this.login = login;
        this.name = name;
        this.picture = picture;
        this.ref = ref;
    }

    handleAuthError(reason) {
        if (reason == "unknown_ref") {
            this.resetUserData();
        }
        console.error(`Auth error: ${reason}`);
    }

    handleTokenExpired() {
        this.resetUserData();
    }

    signOut() {
        this.resetUserData();
        MZBenchWS.send({cmd: "sign-out"});
    }

    requestToken(tokenLifetime) {
        MZBenchWS.send({cmd: "generate-token", lifetime: tokenLifetime.toString(), ref: this.ref})
    }

    handleGeneratedToken(token) {
        this.generatedToken = token;
    }

    getGeneratedToken() {
        return this.generatedToken;
    }

    resetUserData() {
        this.type = "";
        this.ref = "";
        this.login = "";
        this.picture = "";
        this.name = "";
        this.generatedToken = "";
    }
}

var _AuthStore = new AuthStore();
export default _AuthStore;

_AuthStore.dispatchToken = Dispatcher.register((action) => {
    switch (action.type) {
        case ActionTypes.AUTH_REQ:
            _AuthStore.handleAuthReq(action.support);
            _AuthStore.emitChange();
            break;
        case ActionTypes.AUTHENTICATED:
            _AuthStore.handleAuthOk(action.login, action.login_type,
                                      action.name, action.picture_url,
                                      action.ref);
            _AuthStore.emitChange();
            break;

        case ActionTypes.AUTH_ERROR:
            _AuthStore.handleAuthError(action.reason);
            _AuthStore.emitChange();
            break;

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

