-module(mzb_api_ws_handler_tests).

-include_lib("eunit/include/eunit.hrl").

normalize_test() ->
    BenchInfos = [
        {1, #{ status => failed,
               metrics => metrics,
               id => 1,
               start_time => 1437549842,
               config => #{script => #{body => script_body, name => "script_name.erl"}}}},

        {2, #{ status => success,
               metrics => metrics,
               id => 2,
               start_time => 1437549842,
               finish_time => 1437549842,
               config => #{script => #{body => script_body1, name => "another_name.erl"}}}}
    ],

    Normalized = mzb_api_ws_handler:normalize(BenchInfos),

    ?assertEqual([
        #{finish_time => "2015-07-22T07:24:02Z",
          id => 2,
          metrics => metrics,
          script_body => script_body1,
          script_name => "another_name.erl",
          start_time => "2015-07-22T07:24:02Z",
          status => success},
        #{id => 1,
          metrics => metrics,
          script_body => script_body,
          script_name => "script_name.erl",
          start_time => "2015-07-22T07:24:02Z",
          status => failed}],
        Normalized).

filter_test() ->
    Bench = #{finish_time => "2015-07-22T07:24:02Z",
              id => 333,
              metrics => metrics,
              script_body => script_body1,
              script_name => "another_name.erl",
              start_time => "2015-07-22T07:24:02Z",
              status => success},

    ?assertEqual([Bench], mzb_api_ws_handler:apply_filter(#{<<"q">> => <<"">>}, [Bench])),
    ?assertEqual([Bench], mzb_api_ws_handler:apply_filter(#{<<"q">> => <<"20\\d5">>}, [Bench])),
    ?assertEqual([Bench], mzb_api_ws_handler:apply_filter(#{<<"q">> => <<"success">>}, [Bench])),
    ?assertEqual([Bench], mzb_api_ws_handler:apply_filter(#{<<"q">> => <<"another">>}, [Bench])),
    ?assertEqual([Bench], mzb_api_ws_handler:apply_filter(#{<<"q">> => <<"333">>}, [Bench])),

    ?assertEqual([], mzb_api_ws_handler:apply_filter(#{<<"q">> => <<"failed">>}, [Bench])),
    ?assertEqual([], mzb_api_ws_handler:apply_filter(#{<<"q">> => <<"foobar">>}, [Bench])).


pagination_test() ->
    % we use only ids to check limits functionality
    BenchIds = [#{id => Id} || Id <- lists:seq(100, 1, -1)],
    Req = #{<<"limit">> => 2},

    ?assertEqual({[#{id => 100}, #{id => 99}], {99, undefined}},
                 mzb_api_ws_handler:apply_pagination(Req, BenchIds)),

    ?assertEqual({[#{id => 100}, #{id => 99}], {99, undefined}},
                 mzb_api_ws_handler:apply_pagination(Req#{<<"bench_id">> => 100}, BenchIds)),

    % we could show bench on the first page
    ?assertEqual({[#{id => 100}, #{id => 99}], {99, undefined}},
                 mzb_api_ws_handler:apply_pagination(Req#{<<"bench_id">> => 99}, BenchIds)),

    % we are not able to show bench on the first page. Paginate to second page
    ?assertEqual({[#{id => 98}, #{id => 97}], {97, 98}},
                 mzb_api_ws_handler:apply_pagination(Req#{<<"bench_id">> => 98}, BenchIds)),

    ?assertEqual({[#{id => 1}], {undefined, 1}},
                 mzb_api_ws_handler:apply_pagination(Req#{<<"bench_id">> => 1}, BenchIds)),

    ?assertEqual({[#{id => 100}, #{id => 99}], {99, undefined}},
                 mzb_api_ws_handler:apply_pagination(Req#{<<"max_id">> => 200}, BenchIds)),

    ?assertEqual({[#{id => 89}, #{id => 88}], {88, 89}},
                 mzb_api_ws_handler:apply_pagination(Req#{<<"max_id">> => 90}, BenchIds)),

    ?assertEqual({[#{id => 99}, #{id => 98}], {98, 99}},
                 mzb_api_ws_handler:apply_pagination(Req#{<<"max_id">> => 100}, BenchIds)),

    ?assertEqual({[#{id => 100}], {100, undefined}},
                 mzb_api_ws_handler:apply_pagination(Req#{<<"min_id">> => 99}, BenchIds)),

    ?assertEqual({[#{id => 22}, #{id => 21}], {21, 22}},
                 mzb_api_ws_handler:apply_pagination(Req#{<<"min_id">> => 20}, BenchIds)).

