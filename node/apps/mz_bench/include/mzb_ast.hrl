-record(operation, {
        is_std = true :: true | false,
        name = undefined :: atom(),
        args = [] :: abstract_expr(),
        meta = [] :: meta()
        }).

-record(constant, {
        value = undefined :: term(),
        units = undefined :: atom(),
        meta = [] :: meta()
        }).

-record(ramp, {
        curve_type = linear :: linear,
        from :: #constant{},
        to :: #constant{},
        meta = [] :: meta()
        }).
