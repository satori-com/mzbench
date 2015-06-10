function partition(col, length) {
    var result = [];
    for(var i = 0; i < col.length; i++) {
        if(i % length === 0) result.push([]);
        result[result.length - 1].push(col[i]);
    }
    return result;
};
