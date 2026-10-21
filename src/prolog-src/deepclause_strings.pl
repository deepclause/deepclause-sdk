/**
 * deepclause_strings.pl - String interpolation for DeepClause SDK
 * 
 * Provides:
 * - interpolate_string/3: Replace {Variable} patterns with values
 */

:- module(deepclause_strings, [
    interpolate_string/3,
    dml_string_expand/3,
    dc_interp_value/2
]).

:- use_module(library(http/json)).

%% interpolate_string(+Template, +Bindings, -Result)
%% Replace {VariableName} patterns in Template with values from Bindings
%% Bindings is a list of Name=Value pairs
interpolate_string(Template, Bindings, Result) :-
    (   (string(Template) ; atom(Template))
    ->  (   string(Template)
        ->  atom_string(TemplateAtom, Template)
        ;   TemplateAtom = Template
        ),
        interpolate_atom(TemplateAtom, Bindings, ResultAtom),
        atom_string(ResultAtom, Result)
    ;   % If not string or atom, convert to string first
        format(string(S), "~w", [Template]),
        interpolate_string(S, Bindings, Result)
    ).

%% interpolate_atom(+Template, +Bindings, -Result)
interpolate_atom(Template, Bindings, Result) :-
    atom_codes(Template, Codes),
    interpolate_codes(Codes, Bindings, ResultCodes),
    atom_codes(Result, ResultCodes).

%% interpolate_codes(+Codes, +Bindings, -ResultCodes)
%% Process character codes, replacing {var} patterns
interpolate_codes([], _, []) :- !.

% Found opening brace - replace only identifier-like placeholders
interpolate_codes([0'{|Rest], Bindings, Result) :-
    extract_var_name(Rest, VarNameCodes, AfterVar),
    valid_placeholder_codes(VarNameCodes),
    !,
    atom_codes(VarName, VarNameCodes),
    (   find_binding(VarName, Bindings, Value)
    ->  dc_interp_value(Value, ValueString),
        string_codes(ValueString, ValueCodes),
        interpolate_codes(AfterVar, Bindings, RestResult),
        append(ValueCodes, RestResult, Result)
    ;   % Variable not found - keep as is
        interpolate_codes(AfterVar, Bindings, RestResult),
        append([0'{|VarNameCodes], [0'}|RestResult], Result)
    ).

% Invalid placeholder syntax - keep the opening brace literal
interpolate_codes([0'{|Rest], Bindings, [0'{|Result]) :-
    !,
    interpolate_codes(Rest, Bindings, Result).

% Regular character - keep it
interpolate_codes([C|Rest], Bindings, [C|Result]) :-
    interpolate_codes(Rest, Bindings, Result).

%% extract_var_name(+Codes, -VarNameCodes, -RemainingCodes)
%% Extract characters until closing brace
extract_var_name([0'}|Rest], [], Rest) :- !.
extract_var_name([C|Rest], [C|VarRest], Final) :-
    C \= 0'},
    extract_var_name(Rest, VarRest, Final).
extract_var_name([], [], []).  % Handle unclosed brace gracefully

%% valid_placeholder_codes(+Codes)
%% Placeholder names must look like identifiers: [A-Za-z_][A-Za-z0-9_]*
valid_placeholder_codes([First|Rest]) :-
    valid_placeholder_start(First),
    maplist(valid_placeholder_char, Rest).

valid_placeholder_start(Code) :-
    code_type(Code, alpha)
    ; Code =:= 0'_.

valid_placeholder_char(Code) :-
    code_type(Code, alnum)
    ; Code =:= 0'_.

%% dc_interp_value(+Value, -Text)
%% How `{Var}` renders inside a prompt or an answer.
%%
%% Scalars keep their old `~w` spelling (`Ada`, `30`, `positive`). Lists and
%% dicts render as compact JSON instead of Prolog syntax, because the old output
%% (`[_42{score:3,title:A}]`, `[x,y]`) is not what a model reads well — agents
%% wrote `findall`/`format`/`atomic_list_concat` glue just to fix it up.
dc_interp_value(Value, Text) :-
    (   string(Value)
    ->  Text = Value
    ;   number(Value)
    ->  number_string(Value, Text)
    ;   atom(Value)
    ->  atom_string(Value, Text)
    ;   ( is_dict(Value) ; is_list(Value) )
    ->  interp_json(Value, Text)
    ;   format(string(Text), '~w', [Value])
    ).

%% interp_json(+Value, -Text): compact JSON, falling back to `~w` when the term
%% has no JSON form (an unbound variable, a compound such as foo(a)).
%% The JSON writer is called through its library module: this predicate runs
%% inside the meta-interpreter's module too, where an unqualified imported
%% predicate is not visible.
interp_json(Value, Text) :-
    catch(with_output_to(string(Text), json:json_write_dict(current_output, Value, [])), _, fail),
    !.
interp_json(Value, Text) :-
    format(string(Text), '~w', [Value]).

%% find_binding(+VarName, +Bindings, -Value)
%% Look up a variable in the bindings list
find_binding(VarName, Bindings, Value) :-
    (   member(VarName=Value, Bindings)
    ->  true
    ;   % Also try with atom version
        atom_string(VarNameAtom, VarName),
        member(VarNameAtom=Value, Bindings)
    ;   % Try dict-style binding
        is_dict(Bindings),
        get_dict(VarName, Bindings, Value)
    ).

%% dml_string_expand(+Term, +Bindings, -Expanded)
%% Expand strings within a term
dml_string_expand(Term, Bindings, Expanded) :-
    (   string(Term)
    ->  interpolate_string(Term, Bindings, Expanded)
    ;   atom(Term)
    ->  interpolate_string(Term, Bindings, ExpandedStr),
        atom_string(Expanded, ExpandedStr)
    ;   is_list(Term)
    ->  maplist({Bindings}/[T, E]>>dml_string_expand(T, Bindings, E), Term, Expanded)
    ;   compound(Term)
    ->  Term =.. [Functor|Args],
        maplist({Bindings}/[A, EA]>>dml_string_expand(A, Bindings, EA), Args, ExpandedArgs),
        Expanded =.. [Functor|ExpandedArgs]
    ;   Expanded = Term
    ).
