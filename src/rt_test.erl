-module(rt_test).

-compile([export_all]).
-compile({parse_transform, record_transform}).

-record(rt_test, {field_a :: integer(),
                  field_b}).

%default() ->
%    #rt_test{}.

%field_a(R) ->
%    R#rt_test.field_a.

%field_a(R, Val) when is_integer(Val) ->
%    R#rt_test{field_a = Val}.
