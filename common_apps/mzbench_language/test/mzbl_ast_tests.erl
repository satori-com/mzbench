-module(mzbl_ast_tests).
-include_lib("eunit/include/eunit.hrl").
-include("mzbl_types.hrl").

transform_test() ->
  ?assertEqual([], mzbl_ast:transform({nil,1})).

transform2_test() ->
  Input = {cons,1,
            {tuple,1,[{atom,1,size},
                      {integer,1,3}]},
            {cons,2,
             {tuple,2,[{atom,2,worker_type},
                       {atom,2,dummy_worker}]},
             {nil,2}}},
  Output = [#operation{name = size, args = [3], meta = [{line, 1}], is_std = false},
            #operation{name = worker_type, args = [dummy_worker], meta = [{line, 2}], is_std = false}],
  ?assertEqual(Output, mzbl_ast:transform(Input)).

transform_ramp_test() ->
  Input = {cons,1,
             {tuple,2,[{atom,2,ramp},
                       {atom,2,linear},
                       {tuple,2,[{integer,2,30}, {atom,2,rpm}]},
                       {tuple,2,[{integer,2,2}, {atom,2,rps}]}]},
             {nil,2}},
  Output = [#ramp{curve_type = linear,
                  from = #constant{value = 0.5, units = rps},
                  to = #constant{value = 2, units = rps},
                  meta = [{line, 2}]}],
  ?assertEqual(Output, mzbl_literals:convert(mzbl_ast:transform(Input))).
