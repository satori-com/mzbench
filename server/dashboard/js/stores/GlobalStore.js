import { EventEmitter } from 'events';
import Dispatcher from '../dispatcher/AppDispatcher';
import ActionTypes from '../constants/ActionTypes';

const CHANGE_EVENT = 'global_change';

let data = {
    dashboardMode:false,
    tags:[]
};

class GlobalStore extends EventEmitter {
    emitChange() {
        return this.emit(CHANGE_EVENT);
    }

    onChange(callback) {
        this.on(CHANGE_EVENT, callback);
    }

    off(callback) {
        this.removeListener(CHANGE_EVENT, callback);
    }

    isDashboardModeOn() {
        return data.dashboardMode;
    }

    getAllTags() {
        return data.tags;
    }

    dashboardModeOn() {
        if (!data.dashboardMode) {
            data.dashboardMode = true;
            return true;
        }
        return false;
    }

    dashboardModeOff() {
        if (data.dashboardMode) {
            data.dashboardMode = false;
            return true;
        }
        return false;
    }
}

var _GlobalStore = new GlobalStore();
export default _GlobalStore;

_GlobalStore.dispatchToken = Dispatcher.register((action) => {
    switch (action.type) {
        case ActionTypes.TURN_ON_DASHBOARD_MODE:
            if (_GlobalStore.dashboardModeOn())
                _GlobalStore.emitChange();
            break;
        case ActionTypes.TURN_OFF_DASHBOARD_MODE:
            if (_GlobalStore.dashboardModeOff())
                _GlobalStore.emitChange();
            break;
        case ActionTypes.SERVER_INFO:
            data.tags = action.data.tags;
            _GlobalStore.emitChange();
            break;
        default:
    }
});

