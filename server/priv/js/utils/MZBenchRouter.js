import { Router } from 'director';
import MZBenchActions from '../actions/MZBenchActions';
import BenchStore from '../stores/BenchStore';

const routes = {
    '/bench/:benchId/:activeTab': (benchId, activeTab) => {
        MZBenchActions.selectBenchById(benchId);

        // If 'back' button is pressed then old bench could be not
        // presented in the currently loaded timeline
        if (!BenchStore.getSelectedBench() && !isNaN(benchId)) {
            MZBenchActions.getTimeline({bench_id: parseInt(benchId)});
        }

        MZBenchActions.selectActiveTab(activeTab);
    },
    '/timeline': () => {
        let opts = Object.assign({q: ""}, _MZBenchRouter.getQuery());
        MZBenchActions.applyQueryParameters(opts);
        MZBenchActions.getTimeline({});
    }
};

class MZBenchRouter extends Router {
    stringifyQuery(obj) {
        return Object.keys(obj).sort().reduce(function(parts, key) {
            const val = obj[key];
            if (undefined !== val) {
                parts.push(encodeURI(key) + '=' + encodeURI(val));
            }
            return parts;
        }, []).join('&');
    }

    getQuery() {
        let path = this.history === true ? this.getPath() : document.location.hash;
        return this.parseQuery(path.split('?')[1]);
    }

    parseQuery (str) {
        str = (str || "").trim();

        return str.split('&').reduce(function (ret, param) {
            const [key, val, ...rest] = param.split('=');
            ret[decodeURIComponent(key)] = decodeURIComponent(val);
            return ret;
        }, {});
    }

    buildLink(route, queryParams) {
        let queryString = this.stringifyQuery(queryParams);
        queryString = queryString ? "?" + queryString : "";
        return route + queryString;
    }

    navigate(route, queryParams) {
        let link = this.buildLink(route, queryParams);
        this.setRoute(link);
    }
}

let _MZBenchRouter =  new MZBenchRouter(routes);
_MZBenchRouter.init();

export default _MZBenchRouter;
