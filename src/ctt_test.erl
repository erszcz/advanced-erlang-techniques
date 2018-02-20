-module(ctt_test).

-compile([export_all]).
-compile({parse_transform, call_trace_transform}).

io_format_example() ->
    io:format("call ~s/~p\n", [io_format_example, 0]).

guarded_io_format_example() ->
    case get(trace_calls) of
        true ->
            io:format("call ~s/~p\n", [io_format_example, 0]);
        _ -> ok
    end.

start() ->
    123.

stop() ->
    ok.
