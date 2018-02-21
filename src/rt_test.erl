-module(rt_test).

-compile([export_all]).
-compile({parse_transform, record_transform}).

-record(rt_test, {field_a :: integer() | float(),
                  field_b,
                  field_c :: list(byte())}).

%default() ->
%    #rt_test{}.

%field_a(R) ->
%    R#rt_test.field_a.

%field_a(R, Val) when is_integer(Val) ->
%    R#rt_test{field_a = Val}.
