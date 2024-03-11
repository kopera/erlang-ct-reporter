-record(testcase, {
    started_at :: timestamp(),
    ended_at :: timestamp() | undefined,
    ended_with :: success
        | {failure, #{reason := term(), message := iodata(), details => iodata(), stacktrace => list()}}
        | {skip, #{message := iodata(), details => iodata(), stacktrace => list()}}
        | {error, #{message := iodata(), details => iodata(), stacktrace => list()}} | undefined,
    metadata :: map()
}).

-record(suite, {
    started_at :: timestamp(),
    ended_at :: timestamp() | undefined,
    metadata :: map(),
    testcases = #{} :: #{nonempty_list(atom()) => #testcase{}}
}).

-record(report, {
    started_at :: timestamp(),
    ended_at :: timestamp() | undefined,
    suites = #{} :: #{atom() => #suite{}}
}).

-type timestamp() :: {System :: integer(), Monotonic :: integer()}.