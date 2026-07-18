/**
 * deepclause_strings.pl - String interpolation for DeepClause SDK
 * 
 * Provides:
 * - interpolate_string/3: Replace {Variable} patterns with values
 */

:- module(deepclause_strings, [
    interpolate_string/3,
    dml_string_expand/3
]).

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
    ->  format(atom(ValueAtom), '~w', [Value]),
        atom_codes(ValueAtom, ValueCodes),
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
