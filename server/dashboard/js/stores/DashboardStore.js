import moment from 'moment';
import { EventEmitter } from 'events';
import Dispatcher from '../dispatcher/AppDispatcher';
import ActionTypes from '../constants/ActionTypes';

const CHANGE_EVENT = 'dashboard_change';

const defaultData = {
    dashboards: [],
    filter: "",
    pager: {},
    benchset: [],
    benchsetId: 0,
    currentPage: new Map(),
    isLoaded: false,
    isNewSelected: false,
    isNewActive: false,
    newDashboard: {
        name: "Print performance",
        criteria: "#new",
        charts: [{metric: "print.rps",
                  kind: "compare",
                  size: "5",
                  group_env: "",
                  x_env: "",
                  description: ""}]},
    selectedDashboardId: undefined,
    isShowTimelineLoadingMask: false,
    activeTab: undefined,
    activeGraph: undefined
};

let data = jQuery.extend(true, {}, defaultData); // extend is used to copy object recursively


class DashboardStore extends EventEmitter {
    emitChange() {
        return this.emit(CHANGE_EVENT);
    }

    onChange(callback) {
        this.on(CHANGE_EVENT, callback);
    }

    off(callback) {
        this.removeListener(CHANGE_EVENT, callback);
    }

    findById(id) {
        return data.dashboards.find(x => x.id == id);
    }

    updateItem(dashboard) {
        let existDashboard = this.findById(dashboard.id);
        if (existDashboard) {
            Object.assign(existDashboard, dashboard);
        } else {
            data.dashboards.unshift(dashboard);
        }
    }

    loadAll(dashboards) {
        dashboards.sort((a, b) => b.id - a.id);
        data.dashboards = dashboards;
        data.isLoaded = true;
        if (!this.getSelected() && (0 < data.dashboards.length)) {
            data.selectedDashboardId = data.dashboards[0].id;
        }
    }

    getItems() {
        return data.dashboards;
    }

    getSelected() {
        if (!this.isLoaded() || this.isNewSelected()) {
            return undefined;
        }
        return this.findById(data.selectedDashboardId);
    }

    getSelectedId() {
        return data.selectedDashboardId;
    }

    getActiveTab() {
        return data.activeTab;
    }

    getSelectedGraph() {
        return data.activeGraph;
    }

    isLoaded() {
        return data.isLoaded;
    }

    isNewSelected() {
        return data.isNewSelected;
    }

    isNewActive() {
        return data.isNewActive;
    }

    getNew() {
        return data.newDashboard;
    }

    getBenchset() {
        return data.benchset;
    }

    getBenchsetId() {
        return data.benchsetId;
    }

    resetNew() {
        data.newDashboard = Object.assign({}, defaultData.newDashboard);
    }

    isShowTimelineLoadingMask() {
        return data.isShowTimelineLoadingMask;
    }

    getFilter() {
        return data.filter;
    }

    getPager() {
        return data.pager;
    }

    getCurrentPage() {
        return data.currentPage;
    }
}

var _DashboardStore = new DashboardStore();
export default _DashboardStore;

_DashboardStore.dispatchToken = Dispatcher.register((action) => {
    switch (action.type) {
        case ActionTypes.UPDATE_DASHBOARD_INFO:
            _BenchStore.updateItem(action.data);
            _BenchStore.emitChange();
            break;

        case ActionTypes.DASHBOARDS:
            _DashboardStore.loadAll(action.data);
            data.pager = action.pager;
            data.isShowTimelineLoadingMask = false;
            _DashboardStore.emitChange();
            break;

        case ActionTypes.SELECT_DASHBOARD_BY_ID:
            data.isNewSelected = false;
            data.selectedDashboardId = parseInt(action.data);
            _DashboardStore.emitChange();
            break;

        case ActionTypes.SELECT_DASHBOARD_ACTIVE_TAB:
            data.activeTab = action.data;
            _DashboardStore.emitChange();
            break;

        case ActionTypes.SELECT_DASHBOARD_GRAPH:
            data.activeGraph = action.data.graphData;
            _DashboardStore.emitChange();
            break;

        case ActionTypes.DESELECT_DASHBOARD_GRAPH:
            data.activeGraph = undefined;
            _DashboardStore.emitChange();
            break;

        case ActionTypes.SHOW_TIMELINE_LOADING_MASK:
            data.isShowTimelineLoadingMask = true;
            _DashboardStore.emitChange();
            break;

        case ActionTypes.HIDE_TIMELINE_LOADING_MASK:
            data.isShowTimelineLoadingMask = false;
            _DashboardStore.emitChange();
            break;

        case ActionTypes.SET_DASHBOARD_FILTER:
            data.filter = action.data;
            _DashboardStore.emitChange();
            break;

        case ActionTypes.MODIFY_SELECTED_DASHBOARD:
            if (data.isNewSelected)
                action.data(data.newDashboard);
            else
                action.data(_DashboardStore.getSelected());
            _DashboardStore.emitChange();
            break;

        case ActionTypes.ADD_CHART_TO_SELECTED_DASHBOARD:
            let newChart = Object.assign({}, defaultData.newDashboard.charts[0]);
            if (data.isNewSelected)
                data.newDashboard.charts.push(newChart);
            else
                _DashboardStore.getSelected().charts.push(newChart);
            _DashboardStore.emitChange();
            break;            

        case ActionTypes.NEW_DASHBOARD:
            data.selectedDashboardId = undefined;
            data.isNewActive = true;
            data.isNewSelected = true;
            _DashboardStore.emitChange();
            break;

        case ActionTypes.DASHBOARD_CREATED:
            data.isNewSelected = false;
            data.isNewActive = false;
            data.selectedDashboardId = parseInt(action.data);
            _DashboardStore.resetNew();
            _DashboardStore.emitChange();
            $.notify({message: "Dashboard created"}, {type: "info"});
            break;

        case ActionTypes.CLEAN_NEW_DASHBOARD:
            data.newDashboard = Object.assign({}, defaultData.newDashboard);
            data.isNewActive = false;
            _DashboardStore.emitChange();
            break;

        case ActionTypes.BENCHSET:
            data.benchsetId = action.benchset_id;
            data.benchset = action.data;
            _DashboardStore.emitChange();
            break;

        default:
    }
});

