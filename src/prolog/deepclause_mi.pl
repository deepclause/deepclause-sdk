/**
 * deepclause_mi.pl - Meta-interpreter for DeepClause SDK
 * 
 * The simplified meta-interpreter that handles:
 * - task/1 and task/N predicates (agent loops)
 * - exec/2 predicate (external tool calls)
 * - Memory predicates (system, user) - now backtrackable via state
 * - Output predicates (answer, yield, log)
 * - Parameter handling
 * 
 * Key design decisions:
 * - No @-predicates - all LLM interaction via task()
 * - Cooperative execution via engine yields
 * - BACKTRACKABLE MEMORY via state threading (no external dynamic predicates)
 * - State = state{memory: [...], params: {...}}
 */

:- module(deepclause_mi, [
    parse_dml/4,
    create_engine/4,
    step_engine/4,
    destroy_engine/1,
    post_agent_result/3,
    post_exec_result/3,
    provide_input/2,
    mi/3
]).

:- use_module(deepclause_strings).

%% Allow mi_call/3 clauses to be non-contiguous
:- discontiguous mi_call/3.

%% Dynamic predicates for session state (engine management only, not memory!)
:- dynamic session_engine/2.       % session_engine(SessionId, Engine)
:- dynamic session_params/2.       % session_params(SessionId, ParamsDict)
:- dynamic session_user_tools/2.   % session_user_tools(SessionId, ToolName)
:- dynamic session_user_tool_schema/3. % session_user_tool_schema(SessionId, ToolName, Schema)
:- dynamic session_pending_input/2. % session_pending_input(SessionId, Input)
:- dynamic session_agent_result/2. % session_agent_result(SessionId, Result)
:- dynamic session_exec_result/2.  % session_exec_result(SessionId, Result)

%% ============================================================
%% DML Parsing
%% ============================================================

%% parse_dml(+FilePath, +SessionId, +MemoryId, -Error)
%% Parse a DML file and load its clauses into the session module
%% Note: MemoryId is kept for API compatibility but no longer used
parse_dml(FilePath, SessionId, _MemoryId, Error) :-
    catch(
        (
            read_file_to_string(FilePath, Code, []),
            parse_dml_string(Code, SessionId),
            Error = none
        ),
        ParseError,
        format(atom(Error), '~w', [ParseError])
    ).

%% parse_dml_string(+Code, +SessionId)
%% Parse DML code from a string
parse_dml_string(Code, SessionId) :-
    open_string(Code, Stream),
    parse_clauses(Stream, SessionId),
    close(Stream).

%% parse_clauses(+Stream, +SessionId)
%% Read and process all clauses from a stream
parse_clauses(Stream, SessionId) :-
    read_term(Stream, Term, [module(SessionId)]),
    (   Term == end_of_file
    ->  true
    ;   process_clause(Term, SessionId),
        parse_clauses(Stream, SessionId)
    ).

%% process_clause(+Term, +SessionId)
%% Process a single clause and add to session
process_clause((:- Directive), SessionId) :-
    !,
    process_directive(Directive, SessionId).

%% Handle tool/2 with description: tool(Head, Description) :- Body
process_clause((tool(ToolHead, Description) :- Body), SessionId) :-
    !,
    extract_tool_schema(ToolHead, Description, ToolName, Schema),
    assertz(session_user_tools(SessionId, ToolName)),
    assertz(session_user_tool_schema(SessionId, ToolName, Schema)),
    % Store the source code for the tool description
    format(string(SourceCode), "tool(~w, ~q) :-~n    ~w.", [ToolHead, Description, Body]),
    assertz(SessionId:tool_source(ToolName, SourceCode)),
    % Assert the tool implementation (use just ToolHead for execution)
    assertz(SessionId:(tool(ToolHead) :- Body)).

%% Handle tool/1 without description: tool(Head) :- Body
process_clause((tool(ToolHead) :- Body), SessionId) :-
    !,
    extract_tool_schema(ToolHead, none, ToolName, Schema),
    assertz(session_user_tools(SessionId, ToolName)),
    assertz(session_user_tool_schema(SessionId, ToolName, Schema)),
    % Store the source code for the tool description
    format(string(SourceCode), "tool(~w) :-~n    ~w.", [ToolHead, Body]),
    assertz(SessionId:tool_source(ToolName, SourceCode)),
    % Assert the tool implementation
    assertz(SessionId:(tool(ToolHead) :- Body)).

process_clause((Head :- Body), SessionId) :-
    !,
    % Regular clause - assert it
    assertz(SessionId:(Head :- Body)).

process_clause(Fact, SessionId) :-
    % Simple fact
    assertz(SessionId:Fact).

%% ============================================================
%% Tool Schema Extraction
%% ============================================================

%% extract_tool_schema(+ToolHead, +Description, -ToolName, -Schema)
extract_tool_schema(ToolHead, Description, ToolName, Schema) :-
    ToolHead =.. [ToolName|Args],
    length(Args, Arity),
    extract_params(Args, 1, Arity, Inputs, Outputs),
    (Description == none -> Desc = "" ; Desc = Description),
    Schema = schema{
        name: ToolName,
        description: Desc,
        inputs: Inputs,
        outputs: Outputs
    }.

%% extract_params(+Args, +Index, +Arity, -Inputs, -Outputs)
extract_params([], _, _, [], []) :- !.
extract_params([_Arg|Rest], Index, Arity, Inputs, Outputs) :-
    format(atom(Name), 'arg~w', [Index]),
    Type = string,
    NextIndex is Index + 1,
    extract_params(Rest, NextIndex, Arity, RestInputs, RestOutputs),
    Param = param{name: Name, type: Type},
    (   Index == Arity
    ->  Inputs = RestInputs, Outputs = [Param|RestOutputs]
    ;   Inputs = [Param|RestInputs], Outputs = RestOutputs
    ).

%% process_directive(+Directive, +SessionId)
process_directive(param(Key, Desc), SessionId) :-
    !,
    assertz(SessionId:param_decl(Key, Desc)).
process_directive(param(Key, Desc, Default), SessionId) :-
    !,
    assertz(SessionId:param_decl(Key, Desc, Default)).
process_directive(_, _).

%% ============================================================
%% Engine Management
%% ============================================================

%% create_engine(+SessionId, +MemoryId, +Params, -Engine)
%% Note: MemoryId kept for API compatibility but state is now internal
create_engine(SessionId, _MemoryId, Params, Engine) :-
    assertz(session_params(SessionId, Params)),
    determine_agent_goal(SessionId, Params, Goal),
    % Create initial state with empty memory and params
    InitialState = state{memory: [], params: Params},
    % Create the engine - pass SessionId and initial state to mi/3
    engine_create(_, 
        deepclause_mi:mi(Goal, InitialState, SessionId),
        Engine),
    assertz(session_engine(SessionId, Engine)).

%% determine_agent_goal(+SessionId, +Params, -Goal)
determine_agent_goal(SessionId, _Params, SessionId:agent_main) :-
    current_predicate(SessionId:agent_main/0), !.
determine_agent_goal(SessionId, Params, Goal) :-
    current_predicate(SessionId:agent_main/1), !,
    dict_pairs(Params, _, Pairs),
    (   Pairs = [_-V1|_]
    ->  Goal = SessionId:agent_main(V1)
    ;   Goal = SessionId:agent_main(_)
    ).
determine_agent_goal(SessionId, Params, Goal) :-
    current_predicate(SessionId:agent_main/2), !,
    dict_pairs(Params, _, Pairs),
    (   Pairs = [_-V1, _-V2|_]
    ->  Goal = SessionId:agent_main(V1, V2)
    ;   Pairs = [_-V1|_]
    ->  Goal = SessionId:agent_main(V1, _)
    ;   Goal = SessionId:agent_main(_, _)
    ).
determine_agent_goal(SessionId, _Params, SessionId:agent_main).

%% step_engine(+SessionId, -Status, -Content, -Payload)
step_engine(SessionId, Status, Content, Payload) :-
    session_engine(SessionId, Engine),
    (   engine_next(Engine, Result)
    ->  process_engine_result(Result, Status, Content, Payload)
    ;   Status = finished,
        Content = '',
        Payload = none
    ).

%% process_engine_result(+Result, -Status, -Content, -Payload)
process_engine_result(output(Text), output, Text, none) :- !.
process_engine_result(log(Text), log, Text, none) :- !.
process_engine_result(answer(Text), answer, Text, none) :- !.
%% Note: Memory is now passed in the payload for the agent loop
process_engine_result(request_agent_loop(Desc, Vars, Tools, Memory), request_agent_loop, '', 
    payload{taskDescription: Desc, outputVars: Vars, userTools: Tools, memory: Memory}) :- !.
process_engine_result(request_exec(Tool, Args), request_exec, '',
    payload{toolName: Tool, args: Args}) :- !.
%% Tool result from inline tool execution
process_engine_result(tool_result(Result), tool_result, '',
    payload{result: Result}) :- !.
process_engine_result(wait_input(Prompt), wait_input, Prompt, none) :- !.
process_engine_result(error(Msg), error, Msg, none) :- !.
process_engine_result(Other, error, Msg, none) :-
    format(atom(Msg), 'Unknown engine result: ~w', [Other]).

%% destroy_engine(+SessionId)
destroy_engine(SessionId) :-
    (   session_engine(SessionId, Engine)
    ->  catch(engine_destroy(Engine), _, true)
    ;   true
    ),
    retractall(session_engine(SessionId, _)),
    retractall(session_params(SessionId, _)),
    retractall(session_user_tools(SessionId, _)),
    retractall(session_user_tool_schema(SessionId, _, _)),
    retractall(session_pending_input(SessionId, _)),
    retractall(session_agent_result(SessionId, _)),
    retractall(session_exec_result(SessionId, _)),
    (   current_module(SessionId)
    ->  catch(
            (   findall(Head, 
                    (current_predicate(SessionId:Name/Arity), 
                     functor(Head, Name, Arity), 
                     clause(SessionId:Head, _)),
                    Heads),
                forall(member(H, Heads), retractall(SessionId:H))
            ),
            _,
            true
        )
    ;   true
    ).

%% ============================================================
%% Result Posting (from JavaScript)
%% ============================================================

%% post_agent_result(+SessionId, +Success, +Variables)
post_agent_result(SessionId, Success, Variables) :-
    assertz(session_agent_result(SessionId, result{success: Success, variables: Variables})),
    session_engine(SessionId, Engine),
    engine_post(Engine, agent_done).

%% post_exec_result(+SessionId, +Status, +Result)
post_exec_result(SessionId, Status, Result) :-
    assertz(session_exec_result(SessionId, result{status: Status, result: Result})),
    session_engine(SessionId, Engine),
    engine_post(Engine, exec_done).

%% provide_input(+SessionId, +Input)
provide_input(SessionId, Input) :-
    assertz(session_pending_input(SessionId, Input)),
    session_engine(SessionId, Engine),
    engine_post(Engine, input_provided).

%% ============================================================
%% Meta-Interpreter Core - STATE THREADED VERSION
%% ============================================================

%% mi(+Goal, +StateIn, +SessionId)
%% Main meta-interpreter entry point
%% State = state{memory: [...], params: {...}}
mi(Goal, StateIn, SessionId) :-
    nb_setval(current_session_id, SessionId),
    catch(
        mi_call(Goal, StateIn, _StateOut),
        Error,
        (   format(atom(ErrMsg), 'Runtime error: ~w', [Error]),
            engine_yield(error(ErrMsg)),
            fail
        )
    ).

%% ============================================================
%% State Helpers
%% ============================================================

%% add_memory(+StateIn, +Role, +Content, -StateOut)
add_memory(StateIn, Role, Content, StateOut) :-
    Message = message{role: Role, content: Content},
    OldMemory = StateIn.memory,
    append(OldMemory, [Message], NewMemory),
    StateOut = StateIn.put(memory, NewMemory).

%% get_memory(+State, -Memory)
get_memory(State, Memory) :-
    Memory = State.memory.

%% get_params(+State, -Params)
get_params(State, Params) :-
    Params = State.params.

%% ============================================================
%% Agent Signal Loop (for inline tool execution)
%% ============================================================

%% handle_agent_signals(+SessionId, +StateIn, -StateOut)
%% Loop to handle signals from the agent loop until agent_done is received.
%% This allows tools to execute inline with the current state.
handle_agent_signals(SessionId, StateIn, StateOut) :-
    engine_fetch(Signal),
    handle_signal(Signal, SessionId, StateIn, StateOut).

%% handle_signal(+Signal, +SessionId, +StateIn, -StateOut)
handle_signal(agent_done, _SessionId, State, State) :- !.
    % Agent finished, return current state

handle_signal(execute_tool(ToolName, Args), SessionId, StateIn, StateOut) :-
    !,
    % Execute tool inline with current state
    call_tool_inline(SessionId, ToolName, Args, StateIn, State1, Result),
    % Yield the result back to JS
    engine_yield(tool_result(Result)),
    % Continue handling signals with updated state
    handle_agent_signals(SessionId, State1, StateOut).

handle_signal(Unknown, SessionId, StateIn, StateOut) :-
    % Unknown signal, log and continue
    format(user_error, "Warning: Unknown signal in agent loop: ~w~n", [Unknown]),
    handle_agent_signals(SessionId, StateIn, StateOut).

%% call_tool_inline(+SessionId, +ToolName, +Args, +StateIn, -StateOut, -Result)
%% Execute a user-defined tool with the current state.
%% The tool body runs through the meta-interpreter, preserving state threading.
call_tool_inline(SessionId, ToolName, Args, StateIn, StateOut, Result) :-
    % Build the tool goal: tool(name(arg1, arg2, ..., Result))
    atom_string(ToolNameAtom, ToolName),
    append(Args, [Result], AllArgs),
    ToolHead =.. [ToolNameAtom|AllArgs],
    ToolGoal = tool(ToolHead),
    % Execute through meta-interpreter with current state
    (   catch(
            mi_call(SessionId:ToolGoal, StateIn, StateOut),
            Error,
            (   format(string(ErrMsg), "Tool error: ~w", [Error]),
                Result = error{message: ErrMsg},
                StateOut = StateIn
            )
        )
    ->  true
    ;   % Tool failed
        Result = error{message: "Tool execution failed"},
        StateOut = StateIn
    ).

%% ============================================================
%% Task Handling
%% ============================================================

%% collect_user_tools(+SessionId, -ToolSchemas)
collect_user_tools(SessionId, ToolSchemas) :-
    findall(
        tool_info{name: Name, schema: Schema, source: Source},
        (
            session_user_tool_schema(SessionId, Name, Schema),
            (SessionId:tool_source(Name, Source) -> true ; Source = "")
        ),
        ToolSchemas
    ).

%% mi_call(task(Desc), +StateIn, -StateOut)
mi_call(task(Desc), StateIn, StateOut) :-
    !,
    get_params(StateIn, Params),
    interpolate_desc(Desc, Params, InterpDesc),
    get_session_id(SessionId),
    collect_user_tools(SessionId, UserTools),
    get_memory(StateIn, Memory),
    % Yield request with current memory
    engine_yield(request_agent_loop(InterpDesc, [], UserTools, Memory)),
    % Signal loop: handle tool execution requests until agent_done
    handle_agent_signals(SessionId, StateIn, StateAfterAgent),
    session_agent_result(SessionId, Result),
    retract(session_agent_result(SessionId, _)),
    Result.success == true,
    % Add task description as user message and response as assistant
    add_memory(StateAfterAgent, user, InterpDesc, State1),
    (   get_dict(response, Result, Response), Response \= ""
    ->  add_memory(State1, assistant, Response, StateOut)
    ;   StateOut = State1
    ).

%% mi_call(task(Desc, Var1), +StateIn, -StateOut)
mi_call(task(Desc, Var1), StateIn, StateOut) :-
    !,
    mi_call_task_n(Desc, [Var1], ['Var1'], StateIn, StateOut).

%% mi_call(task(Desc, Var1, Var2), +StateIn, -StateOut)
mi_call(task(Desc, Var1, Var2), StateIn, StateOut) :-
    !,
    mi_call_task_n(Desc, [Var1, Var2], ['Var1', 'Var2'], StateIn, StateOut).

%% mi_call(task(Desc, Var1, Var2, Var3), +StateIn, -StateOut)
mi_call(task(Desc, Var1, Var2, Var3), StateIn, StateOut) :-
    !,
    mi_call_task_n(Desc, [Var1, Var2, Var3], ['Var1', 'Var2', 'Var3'], StateIn, StateOut).

%% mi_call_task_n(+Desc, +Vars, +VarNames, +StateIn, -StateOut)
mi_call_task_n(Desc, Vars, VarNames, StateIn, StateOut) :-
    get_params(StateIn, Params),
    interpolate_desc(Desc, Params, InterpDesc),
    get_session_id(SessionId),
    collect_user_tools(SessionId, UserTools),
    get_memory(StateIn, Memory),
    engine_yield(request_agent_loop(InterpDesc, VarNames, UserTools, Memory)),
    % Signal loop: handle tool execution requests until agent_done
    handle_agent_signals(SessionId, StateIn, StateAfterAgent),
    session_agent_result(SessionId, Result),
    retract(session_agent_result(SessionId, _)),
    Result.success == true,
    bind_task_variables(Result.variables, VarNames, Vars),
    % Add task description as user message and response as assistant
    add_memory(StateAfterAgent, user, InterpDesc, State1),
    (   get_dict(response, Result, Response), Response \= ""
    ->  add_memory(State1, assistant, Response, StateOut)
    ;   StateOut = State1
    ).

%% bind_task_variables(+VarsDict, +Names, -Values)
bind_task_variables(_, [], []) :- !.
bind_task_variables(VarsDict, [Name|Names], [Value|Values]) :-
    (   get_dict(Name, VarsDict, Value)
    ->  true
    ;   true
    ),
    bind_task_variables(VarsDict, Names, Values).

%% ============================================================
%% Exec Handling
%% ============================================================

%% mi_call(exec(ToolCall, Output), +StateIn, -StateOut)
mi_call(exec(ToolCall, Output), StateIn, StateIn) :-
    !,
    ToolCall =.. [ToolName|Args],
    get_session_id(SessionId),
    engine_yield(request_exec(ToolName, Args)),
    engine_fetch(_Signal),
    session_exec_result(SessionId, Result),
    retract(session_exec_result(SessionId, _)),
    (   Result.status == success
    ->  Output = Result.result
    ;   fail
    ).

%% ============================================================
%% Memory Predicates - NOW BACKTRACKABLE VIA STATE!
%% ============================================================

%% mi_call(system(Text), +StateIn, -StateOut)
mi_call(system(Text), StateIn, StateOut) :-
    !,
    get_params(StateIn, Params),
    interpolate_desc(Text, Params, InterpText),
    add_memory(StateIn, system, InterpText, StateOut).

%% mi_call(user(Text), +StateIn, -StateOut)
mi_call(user(Text), StateIn, StateOut) :-
    !,
    get_params(StateIn, Params),
    interpolate_desc(Text, Params, InterpText),
    add_memory(StateIn, user, InterpText, StateOut).

%% ============================================================
%% Output Predicates
%% ============================================================

%% mi_call(answer(Text), +StateIn, -StateOut)
mi_call(answer(Text), StateIn, StateIn) :-
    get_params(StateIn, Params),
    interpolate_desc(Text, Params, InterpText),
    engine_yield(answer(InterpText)).

%% mi_call(output(Text), +StateIn, -StateOut)
mi_call(output(Text), StateIn, StateIn) :-
    get_params(StateIn, Params),
    interpolate_desc(Text, Params, InterpText),
    engine_yield(output(InterpText)).

%% mi_call(yield(Text), +StateIn, -StateOut)
mi_call(yield(Text), StateIn, StateIn) :-
    get_params(StateIn, Params),
    interpolate_desc(Text, Params, InterpText),
    engine_yield(output(InterpText)).

%% mi_call(log(Text), +StateIn, -StateOut)
mi_call(log(Text), StateIn, StateIn) :-
    get_params(StateIn, Params),
    interpolate_desc(Text, Params, InterpText),
    engine_yield(log(InterpText)).

%% ============================================================
%% Parameter Handling
%% ============================================================

%% mi_call(param(Key, Desc, Value), +StateIn, -StateOut)
mi_call(param(Key, _Desc, Value), StateIn, StateIn) :-
    !,
    get_params(StateIn, Params),
    (   atom(Key)
    ->  KeyAtom = Key
    ;   atom_string(KeyAtom, Key)
    ),
    get_dict(KeyAtom, Params, Value).

%% ============================================================
%% Control Flow - STATE THREADED
%% No cuts - rely on is_mi_special_predicate guard in catch-all
%% ============================================================

%% mi_call((A, B), +StateIn, -StateOut)
mi_call((A, B), StateIn, StateOut) :-
    mi_call(A, StateIn, State1),
    mi_call(B, State1, StateOut).

%% mi_call((A ; B), +StateIn, -StateOut)
%% Disjunction - BACKTRACKABLE! On failure, StateIn is restored
mi_call((A ; B), StateIn, StateOut) :-
    (   mi_call(A, StateIn, StateOut)
    ;   mi_call(B, StateIn, StateOut)
    ).

%% mi_call((Cond -> Then ; Else), +StateIn, -StateOut)
mi_call((Cond -> Then ; Else), StateIn, StateOut) :-
    (   mi_call(Cond, StateIn, State1)
    ->  mi_call(Then, State1, StateOut)
    ;   mi_call(Else, StateIn, StateOut)
    ).

%% mi_call((Cond -> Then), +StateIn, -StateOut)
mi_call((Cond -> Then), StateIn, StateOut) :-
    (   mi_call(Cond, StateIn, State1)
    ->  mi_call(Then, State1, StateOut)
    ).

%% mi_call(\+(Goal), +StateIn, -StateOut)
mi_call(\+(Goal), StateIn, StateIn) :-
    \+ mi_call(Goal, StateIn, _).

%% mi_call(!, +StateIn, -StateOut)
%% Cut DOES need to actually cut
mi_call(!, StateIn, StateIn) :-
    !.

%% mi_call(true, +StateIn, -StateOut)
mi_call(true, StateIn, StateIn).

%% mi_call(fail, +StateIn, -StateOut)
mi_call(fail, _StateIn, _StateOut) :-
    fail.

%% mi_call(false, +StateIn, -StateOut)
mi_call(false, _StateIn, _StateOut) :-
    fail.

%% ============================================================
%% Catch/Throw - STATE THREADED
%% ============================================================

%% mi_call(catch(Goal, Catcher, Recovery), +StateIn, -StateOut)
mi_call(catch(Goal, Catcher, Recovery), StateIn, StateOut) :-
    catch(
        mi_call(Goal, StateIn, StateOut),
        Catcher,
        mi_call(Recovery, StateIn, StateOut)
    ).

%% mi_call(throw(Error), +StateIn, -StateOut)
mi_call(throw(Error), _StateIn, _StateOut) :-
    !,
    throw(Error).

%% ============================================================
%% Module-Qualified Goals
%% ============================================================

%% mi_call(Module:Goal, +StateIn, -StateOut)
%% Module-qualified goals - use clause/2 for backtracking
mi_call(Module:Goal, StateIn, StateOut) :-
    clause(Module:Goal, Body),
    Body \== true,
    % NO CUT! This allows backtracking through clause/2
    mi_call(Body, StateIn, StateOut).

mi_call(Module:Goal, StateIn, StateIn) :-
    % Fallback for facts or built-ins
    call(Module:Goal).

%% ============================================================
%% Built-in and User-Defined Predicates
%% ============================================================

%% is_mi_special_predicate(+Goal)
%% Check if Goal is a special predicate handled by dedicated mi_call clauses
is_mi_special_predicate(answer(_)).
is_mi_special_predicate(output(_)).
is_mi_special_predicate(yield(_)).
is_mi_special_predicate(log(_)).
is_mi_special_predicate(system(_)).
is_mi_special_predicate(task(_)).
is_mi_special_predicate(task(_,_)).
is_mi_special_predicate(task(_,_,_)).
is_mi_special_predicate(task(_,_,_,_)).
is_mi_special_predicate(task(_,_,_,_,_)).
is_mi_special_predicate(exec(_,_)).
is_mi_special_predicate(param(_,_,_)).
%% Control flow
is_mi_special_predicate((_,_)).
is_mi_special_predicate((_;_)).
is_mi_special_predicate((_->_)).
is_mi_special_predicate((_->_;_)).
is_mi_special_predicate(\+(_)).
is_mi_special_predicate(catch(_,_,_)).
is_mi_special_predicate(throw(_)).
is_mi_special_predicate(!).
is_mi_special_predicate(true).
is_mi_special_predicate(fail).
is_mi_special_predicate(false).
is_mi_special_predicate(_:_).

%% mi_call(Goal, +StateIn, -StateOut)
%% Catch-all for built-in and user-defined predicates
%% MUST NOT match special predicates that have dedicated handlers
mi_call(Goal, StateIn, StateOut) :-
    (   var(Goal)
    ->  throw(error(instantiation_error, mi_call/3))
    ;   true
    ),
    % Skip if this is a special predicate (has dedicated mi_call clause)
    \+ is_mi_special_predicate(Goal),
    mi_call_dispatch(Goal, StateIn, StateOut).

%% mi_call_dispatch(+Goal, +StateIn, -StateOut)
%% Dispatch user-defined vs built-in predicates
%% Uses separate clauses instead of if-then-else to preserve backtracking
mi_call_dispatch(Goal, StateIn, StateIn) :-
    predicate_property(Goal, built_in),
    !,  % Commit: it's built-in, no backtracking needed
    call(Goal).

mi_call_dispatch(Goal, StateIn, StateOut) :-
    get_session_id(SessionId),
    callable(Goal),
    predicate_property(SessionId:Goal, defined),
    % User-defined predicate - use clause/2 for backtracking
    % NO CUT HERE! This allows backtracking through clause/2
    clause(SessionId:Goal, Body),
    mi_call(Body, StateIn, StateOut).

mi_call_dispatch(Goal, StateIn, StateIn) :-
    % Fallback: call directly (external predicates)
    call(Goal).

%% ============================================================
%% Helper Predicates
%% ============================================================

%% interpolate_desc(+Template, +Params, -Result)
interpolate_desc(Template, Params, Result) :-
    (   is_dict(Params)
    ->  dict_pairs(Params, _, Pairs),
        maplist([K-V, K=V]>>true, Pairs, Bindings),
        deepclause_strings:interpolate_string(Template, Bindings, Result)
    ;   Result = Template
    ).

%% get_session_id(-SessionId)
get_session_id(SessionId) :-
    nb_current(current_session_id, SessionId), !.
get_session_id(default_session).
