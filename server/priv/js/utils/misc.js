export default {
    gen_guid() {
        function s4() {
            return Math.floor(0x10000*(1 + Math.random())).toString(16).substring(1);
        }
        
        return s4() + s4() + '-' + s4() + '-' + s4() + '-' + s4() + '-' + s4() + s4() + s4();
    }
}
