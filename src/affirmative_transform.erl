-module(affirmative_transform).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    %io:format("forms: ~p\n", [Forms]),
    parse_trans:plain_transform(fun do_transform/1, Forms).

do_transform({function, Line, Name, Arity, _Clauses}) ->
    Clauses = [{clause, Line, [], [], [{atom, Line, ok}]}],
    {function, Line, Name, Arity, Clauses};

do_transform(_Form) ->
    continue.
