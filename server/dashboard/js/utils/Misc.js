import moment from 'moment';

export default {
    gen_guid() {
        function s4() {
            return Math.floor(0x10000*(1 + Math.random())).toString(16).substring(1);
        }
        
        return s4() + s4() + '-' + s4() + '-' + s4() + '-' + s4() + '-' + s4() + s4() + s4();
    },
    
    sec_to_iso_8601(seconds) {
        return moment.unix(seconds).toISOString();
    },

    ucfirst(string) {
        return string ? string[0].toUpperCase() + string.slice(1) : string;
    },

    uniq_fast(a) {
        let seen = {};
        let out = [];
        let len = a.length;
        let j = 0;
        for(var i = 0; i < len; i++) {
             let item = a[i];
             if(seen[item] !== 1) {
                   seen[item] = 1;
                   out[j++] = item;
             }
        }
        return out;
    }
}
