-module(ct_reporter_junit).

-export([
    id/1,
    init/2,
    terminate/1
]).

-export([
    pre_init_per_suite/3,
    post_end_per_suite/4,
    pre_init_per_group/3,
    post_end_per_group/4,
    pre_init_per_testcase/4,
    post_end_per_testcase/5
]).

-export([
    on_tc_skip/4
]).

-include("./ct_reporter_report.hrl").

-record(state, {
    output :: file:filename_all(),
    hostname :: string(),
    groups :: [atom()], % Used for keeping track of current group for the on_tc_skip callback
    report :: ct_reporter_report:t()
}).

id(Options) ->
    filename:absname(proplists:get_value(path, Options, "junit_report.xml")).


init(ID, _Options) ->
    {ok, _} = application:ensure_all_started(xmerl),
    {ok, Hostname} = inet:gethostname(),
    #state{
        output = ID,
        hostname = Hostname,
        groups = [],
        report = ct_reporter_report:new()
    }.


pre_init_per_suite(SuiteName, Config, #state{hostname = Hostname, report = Report} = State) ->
    Metadata = #{
        hostname => Hostname
    },
    {Config, State#state{
        report = ct_reporter_report:suite_started(SuiteName, Metadata, Report)
    }}.


post_end_per_suite(SuiteName, _Config, Return, #state{report = Report} = State) ->
    {Return, State#state{
        report = ct_reporter_report:suite_ended(SuiteName, Report)
    }}.


pre_init_per_group(GroupName, Config, State) -> 
    {Config, State#state{
        groups = [GroupName | State#state.groups]
    }}.


post_end_per_group(GroupName, _Config, Result, #state{groups = [GroupName | RemainingGroups]} = State) -> 
    {Result, State#state{
        groups = RemainingGroups
    }}.


pre_init_per_testcase(SuiteName, TestCaseName, Config, #state{report = Report} = State) ->
    TestCaseGroups = testcase_groups(Config),
    Metadata = #{},
    {Config, State#state{
        report = ct_reporter_report:testcase_started(SuiteName, TestCaseGroups, TestCaseName, Metadata, Report)
    }}.


post_end_per_testcase(SuiteName, TestCaseName, Config, Return, #state{report = Report} = State) ->
    TestCaseGroups = testcase_groups(Config),
    Status = proplists:get_value(tc_status, Config),
    {Return, State#state{
        report = ct_reporter_report:testcase_ended(SuiteName, TestCaseGroups, TestCaseName, Status, Report)
    }}.


on_tc_skip(SuiteName, {TestCaseName, GroupName}, Reason, #state{groups = [GroupName | _]} = State) ->
    on_tc_skip(SuiteName, TestCaseName, Reason, State);
on_tc_skip(_SuiteName, FunctionName, _Reason, #state{} = State)
        when FunctionName == init_per_suite
            ;FunctionName == end_per_suite
            ;FunctionName == init_per_group
            ;FunctionName == end_per_group ->
    State;
on_tc_skip(SuiteName, TestCaseName, Reason, #state{groups = Groups, report = Report} = State)
        when is_atom(TestCaseName) ->
    TestCaseGroups = lists:reverse(Groups),
    Return = case Reason of
        {tc_auto_skip, R} -> {skipped, R};
        {tc_user_skip, R} -> {skipped, R}
    end,
    State#state{
        report = ct_reporter_report:testcase_ended(SuiteName, TestCaseGroups, TestCaseName, Return, Report)
    }.


terminate(#state{output = Path, report = Report0}) ->
    Report = ct_reporter_report:close(Report0),
    Xml = xmerl:export_simple([to_xmerl(Report)], xmerl_xml),
    {ok, File} = file:open(Path, [write, {encoding, utf8}]),
    io:format(File, "~ts~n", [Xml]),
    file:sync(File),
    file:close(File).


to_xmerl(#report{started_at = StartedAt, ended_at = EndedAt, suites = Suites}) ->
    SortedSuites = lists:sort(fun({_, A}, {_, B}) ->
        A#suite.started_at =< B#suite.started_at
    end, maps:to_list(Suites)),
    {testsuites, [
        {timestamp, timestamp(StartedAt)},
        {time, duration(StartedAt, EndedAt)}
    ], [suite_to_xmerl(SuiteName, Suite) || {SuiteName, Suite} <- SortedSuites]}.

suite_to_xmerl(SuiteName, #suite{metadata = Metadata, started_at = StartedAt, ended_at = EndedAt, testcases = TestCases}) ->
    TotalCount = maps:size(TestCases),
    FailuresCount = maps:fold(fun
        (_Name, #testcase{ended_with = {failure, _}}, Acc) -> Acc + 1;
        (_Name, #testcase{ended_with = _}, Acc) -> Acc
    end, 0, TestCases),
    SkippedCount = maps:fold(fun
        (_Name, #testcase{ended_with = {skip, _}}, Acc) -> Acc + 1;
        (_Name, #testcase{ended_with = _}, Acc) -> Acc
    end, 0, TestCases),
    ErrorsCount = maps:fold(fun
        (_Name, #testcase{ended_with = {error, _}}, Acc) -> Acc + 1;
        (_Name, #testcase{ended_with = _}, Acc) -> Acc
    end, 0, TestCases),
    SortedTestcases = lists:sort(fun({_, A}, {_, B}) ->
        A#testcase.started_at =< B#testcase.started_at
    end, maps:to_list(TestCases)),
    {testsuite, [
        {name, SuiteName},
        {tests, TotalCount},
        {skipped, SkippedCount},
        {failures, FailuresCount},
        {errors, ErrorsCount},
        {time, duration(StartedAt, EndedAt)},
        {hostname, maps:get(hostname, Metadata)},
        {timestamp, timestamp(StartedAt)}
    ], [testcase_to_xmerl(TestCaseGroups, TestCaseName, Testcase) || {{TestCaseGroups, TestCaseName}, Testcase} <- SortedTestcases]}.

testcase_to_xmerl(TestCaseGroups, TestCaseName, #testcase{started_at = StartedAt, ended_at = EndedAt, ended_with = EndedWith}) ->
    {testcase, [
        {name, testcase_path(TestCaseGroups, TestCaseName)},
        {timestamp, timestamp(StartedAt)},
        {time, duration(StartedAt, EndedAt)}
    ], case EndedWith of
        success ->
            [];
        {failure, #{reason := Reason, message := Message} = Info} ->
            Content = case Info of
                #{details := Details} -> [Details];
                #{} -> [Message]
            end,
            [{failure, [
                {type, io_lib:format("~tw", [Reason])},
                {message, Message}
            ], Content}];
        {skip, #{message := Message}} ->
            [{skipped, [
                {message, Message}
            ], []}];
        {error, #{message := Message} = Info} ->
            Content = case Info of
                #{details := Details} -> [Details];
                #{} -> []
            end,
            [{error, [
                {message, Message}
            ], Content}]
    end}.


duration({_, StartedAt}, {_, EndedAt}) ->
    DurationMilliseconds = erlang:convert_time_unit(EndedAt - StartedAt, native, millisecond),
    DurationSeconds = DurationMilliseconds / erlang:convert_time_unit(1, second, millisecond),
    erlang:float_to_list(DurationSeconds, [short]).


timestamp({SystemTime, _}) ->
    calendar:system_time_to_rfc3339(SystemTime, [
        {offset, "Z"},
        {unit, native}
    ]).


testcase_groups(Config) ->
    GroupProperties = proplists:get_value(tc_group_properties, Config, []),
    case proplists:get_all_values(name, GroupProperties) of
        [] ->
            [];
        [Name] ->
            BasePath = lists:foldl(fun (P, Acc) ->
                case proplists:get_all_values(name, P) of
                    [N] -> [N | Acc];
                    [] -> Acc
                end
            end, [], proplists:get_value(tc_group_path, Config, [])),
            lists:reverse([Name | BasePath])
    end.

testcase_path(Groups, Name) ->
    Path = Groups ++ [Name],
    lists:join($., [atom_to_binary(Token) || Token <- Path]).