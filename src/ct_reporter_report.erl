%% @private
-module(ct_reporter_report).
-export([
    new/0,
    suite_started/3,
    suite_ended/2,
    testcase_started/5,
    testcase_ended/5,
    close/1
]).
-export_type([
    t/0
]).

-include("./ct_reporter_report.hrl").
-type t() :: #report{}.


new() ->
    #report{
        started_at = timestamp(),
        suites = #{}
    }.


suite_started(SuiteName, Metadata, #report{suites = Suites} = Report) ->
    Report#report{
        suites = Suites#{
            SuiteName => #suite{
                started_at = timestamp(),
                metadata = Metadata
            }
        }
    }.


suite_ended(SuiteName, #report{suites = Suites} = Report) ->
    Report#report{
        suites = maps:update_with(SuiteName, fun (Suite) ->
            Suite#suite{
                ended_at = timestamp()
            }
        end, Suites)
    }.


testcase_started(SuiteName, TestCaseGroups, TestCaseName, Metadata, #report{suites = Suites} = Report) ->
    TestCaseKey = {TestCaseGroups, TestCaseName},
    Report#report{
        suites = maps:update_with(SuiteName, fun (#suite{testcases = TestCases} = Suite) ->
            Suite#suite{
                testcases = TestCases#{
                    TestCaseKey => #testcase{
                        started_at = timestamp(),
                        metadata = Metadata
                    }
                }
            }
        end, Suites)
    }.


testcase_ended(SuiteName, TestCaseGroups, TestCaseName, Result, #report{suites = Suites} = Report) ->
    TestCaseKey = {TestCaseGroups, TestCaseName},
    Report#report{
        suites = maps:update_with(SuiteName, fun (#suite{testcases = TestCases} = Suite) ->
            Suite#suite{
                testcases = maps:update_with(TestCaseKey, fun (Testcase) ->
                    Testcase#testcase{
                        ended_at = timestamp(),
                        ended_with = case Result of
                            ok ->
                                success;
                            {skipped, Reason} ->
                                {skip, skip_info(Reason)};
                            {failed, {undef, [{SuiteName, TestCaseName, _, _} | _] = StackTrace}} ->
                                {error, #{
                                    message => "missing or unexported test case",
                                    details => erl_error:format_exception(error, undef, StackTrace),
                                    stacktrace => StackTrace
                                }};
                            {failed, Reason} ->
                                {failure, failure_info(Reason)}
                        end
                    }
                end, TestCases)
            }
        end, Suites)
    }.

close(#report{} = Report) ->
    Report#report{
        ended_at = timestamp()
    }.


timestamp() ->
    {erlang:system_time(), erlang:monotonic_time()}.


% skip_info({require_failed, {not_available, Key}}) ->
%     #{
%         message => io_lib:format("missing required config: ~w", [Key])
%     };
% skip_info({require_failed_in_suite0, {not_available, Key}}) ->
%     #{
%         message => io_lib:format("missing required config: ~w", [Key])
%     };
% skip_info({failed, {_Suite, init_per_testcase, {timetrap_timeout, _Timeout}}}) ->
%     #{
%         message => "init_per_testcase timed out"
%     };
% skip_info({failed, {_Suite, init_per_testcase, {Reason, StackTrace}}}) when is_list(StackTrace) ->
%     #{
%         message => io_lib:format("init_per_testcase failed: ~w", [Reason]),
%         details => erl_error:format_exception(error, Reason, StackTrace),
%         stacktrace => StackTrace
%     };
% skip_info({failed, {_Suite, init_per_testcase, Reason}}) ->
%     #{
%         message => io_lib:format("init_per_testcase failed: ~w", [Reason]),
%         details => erl_error:format_exception(error, Reason, [])
%     };
skip_info(Reason) when is_list(Reason); is_binary(Reason) ->
    Message = try unicode:characters_to_binary(Reason) of
        M when is_binary(M) -> M;
        _ -> io_lib:format("~tw", [Reason])
    catch
        _:_ -> io_lib:format("~tw", [Reason])
    end,
    #{
        message => Message
    };
skip_info(Reason) ->
    #{
        message => io_lib:format("~tw", [Reason])
    }.

% failure_info({timetrap_timeout, _Timeout}) ->
%     #{
%         reason => timeout,
%         message => "timetrap timeout"
%     };
% failure_info({failed, {_Suite, end_per_testcase, {Reason, StackTrace}}}) when is_list(StackTrace) ->
%     #{
%         reason => Reason,
%         message => "end_per_testcase failed",
%         details => erl_error:format_exception(error, Reason, StackTrace),
%         stacktrace => StackTrace
%     };
% failure_info({failed, {_Suite, end_per_testcase, Reason}}) ->
%     #{
%         reason => Reason,
%         message => "end_per_testcase failed",
%         details => erl_error:format_exception(error, Reason, [])
%     };
% failure_info({timetrap_timeout, Timeout}}) ->
%     failure_info({timetrap_timeout, Timeout});
failure_info({Reason, StackTrace}) when is_list(StackTrace) ->
    (error_info(Reason, StackTrace))#{
        reason => Reason,
        stacktrace => StackTrace
    };
failure_info(Reason) ->
    (error_info(Reason, []))#{
        reason => Reason
    }.

error_info(Reason, StackTrace) ->
    Description = erl_error:format_exception(error, Reason, StackTrace),
    [Title0 | Details] = string:split(Description, <<"\n">>),
    Title = string:chomp(Title0),
    #{
        message => case string:prefix(Title, <<"exception error: ">>) of
            nomatch -> Title;
            Message -> Message
        end,
        details => string:chomp(Details)
    }.