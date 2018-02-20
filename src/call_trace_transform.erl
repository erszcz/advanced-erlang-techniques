-module(call_trace_transform).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    %io:format("forms: ~p\n", [Forms]),
    parse_trans:plain_transform(fun do_transform/1, Forms).

do_transform({function, Line, Name, Arity, Clauses}) ->
    NewClauses = [ insert_trace_call(Name, Arity, C) || C <- Clauses ],
    {function, Line, Name, Arity, NewClauses};

do_transform(_Form) ->
    continue.

insert_trace_call(FName, FArity, {clause, Line, Patterns, Guards, Exprs}) ->
    NewExprs = [guarded_trace_call(Line, FName, FArity) | Exprs],
    {clause, Line, Patterns, Guards, NewExprs}.

guarded_trace_call(Line, Name, Arity) ->
    {'case', Line, 
     {call, Line, {atom, Line, get}, [{atom, Line, trace_calls}]}, 
     [{clause, Line, 
       [{atom, Line, true}], [], [{call, Line, 
                                   {remote, Line, {atom, Line, io}, {atom, Line, format}}, 
                                   [{string, Line, "call ~s/~p\n"}, 
                                    {cons, Line, 
                                     {atom, Line, Name}, 
                                     {cons, Line, {integer, Line, Arity}, {nil, Line}}}]}]}, 
      {clause, Line, [{var, Line, '_'}], [], [{atom, Line, ok}]}]}.
