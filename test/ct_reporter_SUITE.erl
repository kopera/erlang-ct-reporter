-module(ct_reporter_SUITE).

-compile(export_all).
-behaviour(ct_suite).


suite() ->
    [
        {timetrap, {seconds, 30}}
    ].

% init_per_suite(Config) ->
%     Config.

% end_per_suite(_Config) ->
%     ok.

% init_per_group(_GroupName, Config) ->
%     Config.

% end_per_group(_GroupName, _Config) ->
%     ok.

init_per_testcase(skipped_testcase, _Config) ->
    {skip, "Skipped"};

init_per_testcase(_TestCase, Config) ->
    Config.


end_per_testcase(_TestCase, _Config) ->
    hoho.


groups() ->
    [
        {group0, [], [
            successful_testcase,
            skipped_testcase,
            failing_testcase,
            {group, group1}
        ]},
        {group1, [], [
            undefined_test_case
        ]}
    ].

all() -> 
    [
        successful_testcase,
        skipped_testcase,
        failing_testcase,
        {group, group0}
    ].


successful_testcase(_Config) -> 
    timer:sleep(1234),
    ok.

failing_testcase(_Config) -> 
    true = false,
    ok.