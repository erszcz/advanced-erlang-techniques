-module(record_transform).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    %io:format("forms: ~p\n", [Forms]),
    GeneratedForms = generate_record_accessors(Forms),
    Forms ++ GeneratedForms.

generate_record_accessors([{attribute, Line, record, {RName, Fields}} | Forms]) ->
    NewForms = [ [getter(Line, RName, Field),
                  setter(Line, RName, Field)] || Field <- Fields ],
    %NewForms = [ [getter(Line, RName, Field)] || Field <- Fields ],
    [default(Line, RName)] ++ lists:append(NewForms) ++ generate_record_accessors(Forms);

generate_record_accessors([_ | Forms]) -> generate_record_accessors(Forms);
generate_record_accessors([]) -> [].

default(Line, RecordName) ->
    {function, Line, default, 0,
     [{clause, Line, [], [], [{record, 10, RecordName, []}]}]}.

getter(Line, RecordName, {record_field, _, {atom, _, FieldName}}) ->
    {function, Line, FieldName, 1,
     [{clause, Line, 
       [{var, Line, 'R'}], 
       [], 
       [{record_field, Line, {var, Line, 'R'}, RecordName, {atom, Line, FieldName}}]}]};

getter(Line, RecordName, {typed_record_field, {record_field, _, _} = RF, {type, _, _, _}}) ->
    getter(Line, RecordName, RF).

setter(Line, RecordName, {record_field, _, _} = RF) ->
    setter(Line, RecordName, RF, []);

setter(Line, RecordName, {typed_record_field, {record_field, _, _} = RF, {type, _, Type, _}}) ->
    setter(Line, RecordName, RF, [setter_value_guard(Line, Type)]).

setter(Line, RecordName, {record_field, _, {atom, _, FieldName}}, Guards) ->
    {function, Line, FieldName, 2, 
     [{clause, Line, 
       [{var, Line, 'R'}, {var, Line, 'Val'}], 
       Guards, 
       [{record, Line, 
         {var, Line, 'R'}, 
         RecordName, 
         [{record_field, Line, {atom, Line, FieldName}, {var, Line, 'Val'}}]}]}]}.

setter_value_guard(Line, Type) ->
    Guard = case Type of
                integer -> is_integer;
                float -> is_float;
                binary -> is_binary
                          %% ...
            end,
    [{call, Line, {atom, Line, Guard}, [{var, Line, 'Val'}]}].

%{function,15,field_a,2,
%    [{clause,15,
%         [{var,15,'R'},{var,15,'Val'}],
%         [[{call,15,{atom,15,is_integer},[{var,15,'Val'}]}]],
%         [{record,16,
%              {var,16,'R'},
%              rt_test,
%              [{record_field,16,{atom,16,field_a},{var,16,'Val'}}]}]}]}.
