-module(at_test).

-compile([export_all]).
-compile({parse_transform,affirmative_transform}).

start() ->
    123.

stop() ->
    qwe.
