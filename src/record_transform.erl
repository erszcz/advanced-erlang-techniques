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
    getter(Line, RecordName, RF);

getter(Line, RecordName, {typed_record_field, {record_field, _, _} = RF, {user_type, _, _, _}}) ->
    getter(Line, RecordName, RF).

setter(Line, RecordName, {record_field, _, _} = RF) ->
    setter(Line, RecordName, RF, []);

setter(Line, RecordName, {typed_record_field, {record_field, _, _} = RF, TypeNode}) ->
    setter(Line, RecordName, RF, setter_value_guards(Line, TypeNode)).

setter(Line, RecordName, {record_field, _, {atom, _, FieldName}}, Guards) ->
    {function, Line, FieldName, 2, 
     [{clause, Line, 
       [{var, Line, 'R'}, {var, Line, 'Val'}], 
       Guards, 
       [{record, Line, 
         {var, Line, 'R'}, 
         RecordName, 
         [{record_field, Line, {atom, Line, FieldName}, {var, Line, 'Val'}}]}]}]}.

setter_value_guards(Line, {type, _, union, TypeNodes}) ->
    lists:concat([ setter_value_guards(Line, TN) || TN <- TypeNodes ]);

setter_value_guards(Line, {type, _, Type, _}) ->
    case lists:keyfind(Type, 1, basic_types()) of
        false ->
            [];
        {Type, Guard} ->
            [ [{call, Line, {atom, Line, Guard}, [{var, Line, 'Val'}]}] ]
    end.

basic_types() ->
    [{atom, is_atom},
     {float, is_float},
     {integer, is_integer},
     {list, is_list},
     {number, is_number},
     {pid, is_pid},
     {port, is_port},
     {reference, is_reference},
     {tuple, is_tuple},
     {map, is_map},
     {binary, is_binary},
     {function, is_function}].
