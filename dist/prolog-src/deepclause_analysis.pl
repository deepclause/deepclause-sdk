:- module(deepclause_analysis, [
    analyze_source/2,
    analyze_terms/2
]).

:- use_module(library(lists)).
:- use_module(library(ordsets)).

%% analyze_source(+SourceCode, -AnalysisResult)
analyze_source(SourceCode, Result) :-
    catch(
        (
            open_string(SourceCode, Stream),
            read_all_terms(Stream, Terms),
            close(Stream),
            analyze_terms(Terms, Warnings),
            extract_capabilities(Terms, Caps),
            Result = analysis{
                valid: true, 
                warnings: Warnings,
                capabilities: Caps
            }
        ),
        Error,
        (
            format(string(Msg), "Analysis failed: ~w", [Error]),
            Result = analysis{valid: false, error: Msg, warnings: [], capabilities: []}
        )
    ).

read_all_terms(Stream, Terms) :-
    read_term(Stream, T, [variable_names(Vars)]),
    (   T == end_of_file
    ->  Terms = []
    ;   Terms = [term(T, Vars)|Rest],
        read_all_terms(Stream, Rest)
    ).

%% analyze_terms(+TermsList, -Warnings)
analyze_terms(Terms, Warnings) :-
    findall(W, (
        member(term((Head :- Body), Vars), Terms),
        check_clause(Head, Body, Vars, W)
    ), WarningsList),
    flatten(WarningsList, Warnings).

%% check_clause(+Head, +Body, +Vars, -Warning)
check_clause(Head, Body, Vars, Warning) :-
    goals_list(Body, Goals),
    
    % 1. Identify Sources (as Names)
    % Include variables from Head if it's a tool or agent_main
    findall(Name-SourceType, (
        (   (Head = tool(ToolHead, _) ; Head = tool(ToolHead)) -> 
            ToolHead =.. [_|HeadArgs], SourceType = "tool argument"
        ;   functor(Head, agent_main, _) ->
            Head =.. [_|HeadArgs], SourceType = "agent argument"
        ;   fail
        ),
        member(V, HeadArgs),
        var_name(V, Vars, Name)
    ), HeadSources),
    
    findall(Name-SourceType, is_source(Goals, Vars, Name, SourceType), OtherSources),
    append(HeadSources, OtherSources, Sources),
    
    % 2. Propagate Taint (using Names)
    propagate_taint(Goals, Vars, Sources, TaintedNames),
    
    % 3. Check Sinks
    check_sink(Head, Goals, Vars, TaintedNames, Warning).

check_clause(_, Body, _, warning(low, "Avoid using format() directly inside answer(). Use string interpolation.")) :-
    goals_list(Body, Goals),
    member(answer(format(_,_)), Goals).

check_clause(_, Body, _, warning(high, Msg)) :-
    goals_list(Body, Goals),
    member(exec(ToolCall, _), Goals),
    functor(ToolCall, ToolName, _),
    dangerous_tool(ToolName),
    format(string(Msg), "Usage of dangerous tool detected: '~w'", [ToolName]).


%% ============================================================================
%% Flow Analysis
%% ============================================================================

is_source(Goals, Vars, Name, "parameter") :- member(param(_, _, Var), Goals), var_name(Var, Vars, Name).
is_source(Goals, Vars, Name, "parameter") :- member(param(_, Var), Goals), var_name(Var, Vars, Name).
is_source(Goals, Vars, Name, "user input") :- member(input(_, Var), Goals), var_name(Var, Vars, Name).
is_source(Goals, Vars, Name, "user message") :- member(user(Var), Goals), var_name(Var, Vars, Name).
is_source(Goals, Vars, Name, "LLM output") :- member(task(_, Var), Goals), var_name(Var, Vars, Name).
is_source(Goals, Vars, Name, "LLM output") :- member(task(_, _, Var), Goals), var_name(Var, Vars, Name).
is_source(Goals, Vars, Name, "LLM output") :- member(task(_, Var, _), Goals), var_name(Var, Vars, Name).
is_source(Goals, Vars, Name, "LLM output") :- member(task(_, _, _, Var), Goals), var_name(Var, Vars, Name).

propagate_taint(Goals, Vars, TaintedIn, TaintedOut) :-
    findall(NewName-Source, (
        member(Goal, Goals),
        propagates(Goal, Vars, InName, NewName),
        member(InName-Source, TaintedIn)
    ), NewTaints),
    (   NewTaints == []
    ->  TaintedOut = TaintedIn
    ;   append(TaintedIn, NewTaints, NextTaints),
        sort(NextTaints, UniqueNext),
        (   length(UniqueNext, Len1), length(TaintedIn, Len2), Len1 == Len2
        ->  TaintedOut = TaintedIn
        ;   propagate_taint(Goals, Vars, UniqueNext, TaintedOut)
        )
    ).

% Propagation rules with Vars lookup
propagates(format(string(Out), _, Args), Vars, InName, OutName) :- 
    member(InVar, Args), var_name(InVar, Vars, InName),
    var_name(Out, Vars, OutName).
propagates((Out = In), Vars, InName, OutName) :-
    var_name(In, Vars, InName),
    var_name(Out, Vars, OutName).
propagates(string_concat(A, _, Out), Vars, InName, OutName) :- 
    var_name(A, Vars, InName), var_name(Out, Vars, OutName).
propagates(string_concat(_, B, Out), Vars, InName, OutName) :- 
    var_name(B, Vars, InName), var_name(Out, Vars, OutName).
propagates(atom_concat(A, _, Out), Vars, InName, OutName) :- 
    var_name(A, Vars, InName), var_name(Out, Vars, OutName).
propagates(atom_concat(_, B, Out), Vars, InName, OutName) :- 
    var_name(B, Vars, InName), var_name(Out, Vars, OutName).

check_sink(Head, Goals, Vars, TaintedNames, warning(high, Msg)) :-
    member(system(Text), Goals),
    member(Name-Source, TaintedNames),
    var_name(Text, Vars, Name),
    format(string(Msg), "Taint: ~s flows into system prompt in '~w'", [Source, Head]).

check_sink(Head, Goals, Vars, TaintedNames, warning(critical, Msg)) :-
    member(exec(ToolCall, _), Goals),
    % Check if any variable inside ToolCall is tainted
    term_variables(ToolCall, ToolVars),
    member(V, ToolVars),
    var_name(V, Vars, Name),
    member(Name-Source, TaintedNames),
    format(string(Msg), "Security Risk: ~s flows into external tool execution in '~w'", [Source, Head]).

check_sink(Head, Goals, Vars, TaintedNames, warning(medium, Msg)) :-
    member(TextGoal, Goals),
    (TextGoal = user(Text) ; TextGoal = system(Text)),
    member(Name-Source, TaintedNames),
    var_name(Text, Vars, Name),
    % Find if there's a task call that follows
    nth0(IdxSource, Goals, TextGoal),
    member(TaskGoal, Goals),
    (TaskGoal = task(_) ; TaskGoal = task(_,_) ; TaskGoal = task(_,_,_) ; TaskGoal = task_named(_,_,_)),
    nth0(IdxSink, Goals, TaskGoal),
    IdxSink > IdxSource,
    format(string(Msg), "Prompt Injection Risk: Task in '~w' implicitly inherits memory tainted by ~s", [Head, Source]).

%% ============================================================================
%% Helpers
%% ============================================================================

var_name(Var, Vars, Name) :-
    var(Var),
    member(Name=V, Vars),
    V == Var, !.

dangerous_tool(vm_exec).
dangerous_tool(shell_exec).
dangerous_tool(eval).

goals_list(Var, []) :- var(Var), !.
goals_list((A, B), Goals) :- !, goals_list(A, GA), goals_list(B, GB), append(GA, GB, Goals).
goals_list((A; B), Goals) :- !, goals_list(A, GA), goals_list(B, GB), append(GA, GB, Goals).
goals_list((A->B), Goals) :- !, goals_list(A, GA), goals_list(B, GB), append(GA, GB, Goals).
goals_list(Goal, [Goal]).

extract_capabilities(Terms, Caps) :-
    findall(Cap, find_capability(Terms, Cap), Dups),
    sort(Dups, Caps).

find_capability(Terms, tool_use(ToolName)) :-
    member(term((_ :- Body), _), Terms),
    goals_list(Body, Goals),
    member(exec(ToolCall, _), Goals),
    functor(ToolCall, ToolName, _).

find_capability(Terms, file_io) :-
    member(term((_ :- Body), _), Terms),
    goals_list(Body, Goals),
    (member(exec(read_file(_),_), Goals) ; member(exec(write_file(_),_), Goals)).

find_capability(Terms, network) :-
    member(term((_ :- Body), _), Terms),
    goals_list(Body, Goals),
    (member(exec(web_search(_),_), Goals) ; member(exec(fetch(_),_), Goals)).

find_capability(Terms, shell) :-
    member(term((_ :- Body), _), Terms),
    goals_list(Body, Goals),
    member(exec(vm_exec(_),_), Goals).
