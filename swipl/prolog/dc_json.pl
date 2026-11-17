%% JSON and schema helpers for the plain SWI runtime.
:- module(dc_json, [
    json_text/2,
    json_parse/2,
    dict_get/3,
    dict_get/4,
    as_text/2,
    as_atom/2,
    key_atom/2,
    tool_json_schema/2,
    tool_description/2,
    tool_input_specs/3,
    value_json/2,
    result_text/2,
    usage_dict/2
]).

:- use_module(library(http/json)).

%% json_text(+Term, -String)
%% Serialize a Prolog value as JSON. Dicts use the dict writer; lists and
%% primitives use the generic writer.
json_text(Term, Text) :-
    with_output_to(string(Text), json_write_text(Term)).

json_write_text(Term) :-
    (   is_dict(Term)
    ->  json_write_dict(current_output, Term, [])
    ;   json_write(current_output, Term, [])
    ).

%% json_parse(+Text, -Term)
%% Parse one JSON value. Objects become dicts with atom keys; arrays become
%% lists. Trailing whitespace is ignored.
json_parse(Text0, Term) :-
    (   atom(Text0)
    ->  atom_string(Text, Text0)
    ;   string(Text0)
    ->  Text = Text0
    ;   atom_string(Text, Text0)
    ),
    open_string(Text, Stream),
    call_cleanup(
        json_read_dict(Stream, Term, [end_of_term(eof)]),
        close(Stream)
    ).

%% dict_get(+Dict, +Key, -Value) / dict_get(+Dict, +Key, +Default, -Value)
dict_get(Dict, Key, Value) :-
    key_atom(Key, Atom),
    get_dict(Atom, Dict, Value).

dict_get(Dict, Key, Default, Value) :-
    key_atom(Key, Atom),
    (   get_dict(Atom, Dict, Found)
    ->  Value = Found
    ;   Value = Default
    ).

%% as_text(+Value, -String)
as_text(Value, Text) :-
    (   string(Value)
    ->  Text = Value
    ;   atom(Value)
    ->  atom_string(Value, Text)
    ;   number(Value)
    ->  number_string(Value, Text)
    ;   is_dict(Value)
    ->  json_text(Value, Text)
    ;   is_list(Value)
    ->  json_text(Value, Text)
    ;   term_string(Value, Text)
    ),
    !.

%% as_atom(+Value, -Atom)
as_atom(Value, Atom) :-
    (   atom(Value)
    ->  Atom = Value
    ;   string(Value)
    ->  atom_string(Atom, Value)
    ;   number(Value)
    ->  number_string(Value, Text),
        atom_string(Atom, Text)
    ;   term_string(Value, Text),
        atom_string(Atom, Text)
    ),
    !.

%% key_atom(+Key, -Atom)
key_atom(Key, Atom) :-
    (   atom(Key)
    ->  Atom = Key
    ;   string(Key)
    ->  atom_string(Atom, Key)
    ;   Atom = Key
    ),
    !.

%% tool_json_schema(+ToolInfo, -Schema)
%% Convert the MI's tool_info{schema: schema{inputs: [...]}} into JSON Schema.
tool_json_schema(ToolInfo, Json) :-
    is_dict(ToolInfo),
    get_dict(schema, ToolInfo, Schema),
    (   get_dict(inputs, Schema, Inputs)
    ->  true
    ;   Inputs = []
    ),
    tool_input_specs(Inputs, Pairs, Required),
    (   Pairs == []
    ->  dict_create(Properties, _, [])
    ;   dict_create(Properties, _, Pairs)
    ),
    Json = _{type: object, properties: Properties, required: Required}.

tool_input_specs([], [], []).
tool_input_specs([Param|Params], [Name-Prop|Pairs], [Name|Required]) :-
    get_dict(name, Param, RawName),
    key_atom(RawName, Name),
    (   get_dict(type, Param, Type0)
    ->  json_type(Type0, Type)
    ;   Type = string
    ),
    (   get_dict(description, Param, Description0),
        Description0 \== '',
        Description0 \== ""
    ->  as_text(Description0, Description)
    ;   Description = ""
    ),
    Prop = _{type: Type, description: Description},
    tool_input_specs(Params, Pairs, Required).

json_type(string, string) :- !.
json_type(integer, integer) :- !.
json_type(number, number) :- !.
json_type(float, number) :- !.
json_type(boolean, boolean) :- !.
json_type(array, array) :- !.
json_type(list, array) :- !.
json_type(object, object) :- !.
json_type(_, string).

%% tool_description(+ToolInfo, -Text)
tool_description(ToolInfo, Text) :-
    is_dict(ToolInfo),
    get_dict(schema, ToolInfo, Schema),
    (   get_dict(description, Schema, Description)
    ->  as_text(Description, Text)
    ;   Text = ""
    ).

%% value_json(+Value, -JsonValue)
%% Convert a Prolog value for inclusion in a JSON request. Terms that are not
%% JSON-native become strings; this is deliberately conservative.
value_json(Value, Value) :-
    (   string(Value) ; number(Value) ; Value == true ; Value == false ;
        Value == null ; is_list(Value) ; is_dict(Value)
    ),
    !.
value_json(Value, Text) :-
    term_string(Value, Text).

%% result_text(+Result, -Text)
%% A tool result as the text a provider should see.
result_text(Result, Text) :-
    (   string(Result)
    ->  Text = Result
    ;   Result == true
    ->  Text = "true"
    ;   Result == false
    ->  Text = "false"
    ;   Result == null
    ->  Text = "null"
    ;   is_dict(Result)
    ->  json_text(Result, Text)
    ;   is_list(Result)
    ->  json_text(Result, Text)
    ;   number(Result)
    ->  number_string(Result, Text)
    ;   atom(Result)
    ->  atom_string(Result, Text)
    ;   term_string(Result, Text)
    ).

%% usage_dict(+Raw, -Usage)
%% Normalize provider usage JSON to the shape the rest of the runtime uses.
usage_dict(Raw, usage{input: Input, output: Output, total: Total,
                     cache_read: CacheRead, cache_write: CacheWrite,
                     reasoning: Reasoning}) :-
    dict_get(Raw, input_tokens, 0, Input0),
    dict_get(Raw, output_tokens, 0, Output0),
    dict_get(Raw, total_tokens, Total0),
    dict_get(Raw, cache_read, 0, CacheRead0),
    dict_get(Raw, cache_write, 0, CacheWrite0),
    dict_get(Raw, reasoning, 0, Reasoning0),
    integer_or_zero(Input0, Input),
    integer_or_zero(Output0, Output),
    integer_or_zero(CacheRead0, CacheRead),
    integer_or_zero(CacheWrite0, CacheWrite),
    integer_or_zero(Reasoning0, Reasoning),
    (   Total0 == undefined
    ->  Total is Input + Output
    ;   integer_or_zero(Total0, Total)
    ).

integer_or_zero(Value, Value) :- integer(Value), !.
integer_or_zero(Value, 0) :- number(Value), number_integer(Value, 0), !.
integer_or_zero(_, 0).
