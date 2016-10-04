import moment from 'moment';
import { EventEmitter } from 'events';
import Dispatcher from '../dispatcher/AppDispatcher';
import ActionTypes from '../constants/ActionTypes';
import MZBenchWS from '../utils/MZBenchWS';

const CHANGE_EVENT = 'auth_change';

class AuthStore extends EventEmitter {
    constructor() {
        super();
        this.ref = "";
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

    handle_auth_req(support) {
        if (this.ref == "") {
            this.type = null;
            this.login = null;
            this.picture = null;
            this.name = null;
            this.authMethods = support;
        } else {
            MZBenchWS.send({cmd: "my-ref", data: this.ref});
        }

    }

    handle_auth_ok(login, type, name, picture, ref) {
        this.type = type;
        this.login = login;
        this.name = name;
        this.picture = picture;
        this.ref = ref;
    }

    handle_auth_error(reason) {
        console.error(`Auth error: ${reason}`);
    }

    signOut() {
        this.ref = "";
        this.type = null;
        this.login = null;
        this.picture = null;
        this.name = null;
        MZBenchWS.send({cmd: "sign-out"});
    }
}

var _AuthStore = new AuthStore();
export default _AuthStore;

_AuthStore.dispatchToken = Dispatcher.register((action) => {
    switch (action.type) {
        case ActionTypes.AUTH_REQ:
            _AuthStore.handle_auth_req(action.support);
            _AuthStore.emitChange();
            break;
        case ActionTypes.AUTHENTICATED:
            _AuthStore.handle_auth_ok(action.login, action.login_type,
                                      action.name, action.picture_url,
                                      action.ref);
            _AuthStore.emitChange();
            break;

        case ActionTypes.AUTH_ERROR:
            _AuthStore.handle_auth_error(action.reason);
            _AuthStore.emitChange();
            break;

        default:
    }
});

