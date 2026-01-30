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
    parse_dml/5,
    create_engine/5,
    step_engine/4,
    destroy_engine/1,
    post_agent_result/4,
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
:- dynamic session_trace_log/2.    % session_trace_log(SessionId, TraceLog) - accumulated trace entries
:- dynamic session_trace_enabled/2. % session_trace_enabled(SessionId, true/false)

%% ============================================================
%% DML Parsing
%% ============================================================

%% parse_dml(+FilePath, +SessionId, +MemoryId, +Params, -Error)
%% Parse a DML file and load its clauses into the session module
%% Also asserts param/2 facts from the Params dict
parse_dml(FilePath, SessionId, _MemoryId, Params, Error) :-
    catch(
        (
            read_file_to_string(FilePath, Code, []),
            assert_params(SessionId, Params),
            parse_dml_string(Code, SessionId),
            Error = none
        ),
        ParseError,
        format(atom(Error), '~w', [ParseError])
    ).

%% assert_params(+SessionId, +Params)
%% Assert param/2 facts from the Params dict into the session module
assert_params(SessionId, Params) :-
    (   is_dict(Params)
    ->  dict_pairs(Params, _, Pairs),
        forall(
            member(Key-Value, Pairs),
            assertz(SessionId:param(Key, Value))
        )
    ;   true
    ).

%% parse_dml_string(+Code, +SessionId)
%% Parse DML code from a string
parse_dml_string(Code, SessionId) :-
    open_string(Code, Stream),
    parse_clauses(Stream, SessionId),
    close(Stream).

%% parse_clauses(+Stream, +SessionId)
%% Read and process all clauses from a stream
%% Uses variable_names to enable compile-time string interpolation
parse_clauses(Stream, SessionId) :-
    read_term(Stream, Term, [module(SessionId), variable_names(Bindings)]),
    (   Term == end_of_file
    ->  true
    ;   expand_interpolations(Term, Bindings, ExpandedTerm),
        process_clause(ExpandedTerm, SessionId),
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

%% create_engine(+SessionId, +MemoryId, +Args, +Params, -Engine)
%% Args is a list of positional arguments for agent_main
%% Params is a dict of named parameters (already asserted as param/2 facts)
create_engine(SessionId, _MemoryId, Args, Params, Engine) :-
    assertz(session_params(SessionId, Params)),
    determine_agent_goal(SessionId, Args, Goal),
    % Check if tracing is enabled and store in session state
    (get_dict(trace, Params, true) -> TraceEnabled = true ; TraceEnabled = false),
    assertz(session_trace_enabled(SessionId, TraceEnabled)),
    assertz(session_trace_log(SessionId, [])),
    % Create initial state with empty memory, params, context stack, and trace depth
    InitialState = state{memory: [], params: Params, context_stack: [], depth: 0},
    % Create the engine - pass SessionId and initial state to mi/3
    engine_create(_, 
        deepclause_mi:mi(Goal, InitialState, SessionId),
        Engine),
    assertz(session_engine(SessionId, Engine)).

%% determine_agent_goal(+SessionId, +Args, -Goal)
%% Args is a list of positional arguments - passed directly to agent_main
determine_agent_goal(SessionId, _Args, SessionId:agent_main) :-
    current_predicate(SessionId:agent_main/0), !.
determine_agent_goal(SessionId, Args, Goal) :-
    current_predicate(SessionId:agent_main/1), !,
    (   Args = [Arg1|_]
    ->  Goal = SessionId:agent_main(Arg1)
    ;   Goal = SessionId:agent_main(_)
    ).
determine_agent_goal(SessionId, Args, Goal) :-
    current_predicate(SessionId:agent_main/2), !,
    (   Args = [Arg1, Arg2|_]
    ->  Goal = SessionId:agent_main(Arg1, Arg2)
    ;   Args = [Arg1|_]
    ->  Goal = SessionId:agent_main(Arg1, _)
    ;   Goal = SessionId:agent_main(_, _)
    ).
determine_agent_goal(SessionId, Args, Goal) :-
    current_predicate(SessionId:agent_main/3), !,
    (   Args = [Arg1, Arg2, Arg3|_]
    ->  Goal = SessionId:agent_main(Arg1, Arg2, Arg3)
    ;   Args = [Arg1, Arg2|_]
    ->  Goal = SessionId:agent_main(Arg1, Arg2, _)
    ;   Args = [Arg1|_]
    ->  Goal = SessionId:agent_main(Arg1, _, _)
    ;   Goal = SessionId:agent_main(_, _, _)
    ).
determine_agent_goal(SessionId, _Args, SessionId:agent_main).

%% step_engine(+SessionId, -Status, -Content, -Payload)
step_engine(SessionId, Status, Content, Payload) :-
    session_engine(SessionId, Engine),
    (   engine_next(Engine, Result)
    ->  process_engine_result(Result, Status, Content, Payload)
    ;   % Engine finished - return trace if enabled
        Status = finished,
        Content = '',
        (   session_trace_enabled(SessionId, true),
            session_trace_log(SessionId, TraceLog)
        ->  Payload = payload{trace: TraceLog}
        ;   Payload = none
        )
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
    retractall(session_trace_log(SessionId, _)),
    retractall(session_trace_enabled(SessionId, _)),
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

%% post_agent_result(+SessionId, +Success, +Variables, +Messages)
%% Messages is a list of message{role: Role, content: Content} dicts
post_agent_result(SessionId, Success, Variables, Messages) :-
    assertz(session_agent_result(SessionId, result{success: Success, variables: Variables, messages: Messages})),
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
        (   Error == '$answer_commit'
        ->  true  % answer/1 threw to commit - success, no backtracking
        ;   format(atom(ErrMsg), 'Runtime error: ~w', [Error]),
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

%% get_context_stack(+State, -Stack)
get_context_stack(State, Stack) :-
    (   get_dict(context_stack, State, Stack)
    ->  true
    ;   Stack = []
    ).

%% set_context_stack(+StateIn, +Stack, -StateOut)
set_context_stack(StateIn, Stack, StateOut) :-
    StateOut = StateIn.put(context_stack, Stack).

%% set_memory(+StateIn, +Memory, -StateOut)
set_memory(StateIn, Memory, StateOut) :-
    StateOut = StateIn.put(memory, Memory).

%% ============================================================
%% Trace Helpers
%% ============================================================

%% get_depth(+State, -Depth)
get_depth(State, Depth) :-
    (   get_dict(depth, State, Depth)
    ->  true
    ;   Depth = 0
    ).

%% set_depth(+StateIn, +Depth, -StateOut)
set_depth(StateIn, Depth, StateOut) :-
    StateOut = StateIn.put(depth, Depth).

%% add_trace_entry(+SessionId, +Type, +Predicate, +Args, +Depth)
%% Add a trace entry if tracing is enabled
add_trace_entry(SessionId, Type, Predicate, Args, Depth) :-
    (   session_trace_enabled(SessionId, true)
    ->  get_time(Now),
        Timestamp is round(Now * 1000),  % Convert to milliseconds
        Entry = trace{timestamp: Timestamp, type: Type, predicate: Predicate, args: Args, depth: Depth},
        session_trace_log(SessionId, OldLog),
        retract(session_trace_log(SessionId, OldLog)),
        append(OldLog, [Entry], NewLog),
        assertz(session_trace_log(SessionId, NewLog))
    ;   true
    ).

%% add_trace_with_result(+SessionId, +Type, +Predicate, +Args, +Result, +Depth)
%% Add a trace entry with a result field
add_trace_with_result(SessionId, Type, Predicate, Args, Result, Depth) :-
    (   session_trace_enabled(SessionId, true)
    ->  get_time(Now),
        Timestamp is round(Now * 1000),
        Entry = trace{timestamp: Timestamp, type: Type, predicate: Predicate, args: Args, result: Result, depth: Depth},
        session_trace_log(SessionId, OldLog),
        retract(session_trace_log(SessionId, OldLog)),
        append(OldLog, [Entry], NewLog),
        assertz(session_trace_log(SessionId, NewLog))
    ;   true
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
    get_depth(StateIn, Depth),
    add_trace_entry(SessionId, llm_call, task, [InterpDesc], Depth),
    collect_user_tools(SessionId, UserTools),
    get_memory(StateIn, Memory),
    % Yield request with current memory
    engine_yield(request_agent_loop(InterpDesc, [], UserTools, Memory)),
    engine_fetch(_Signal),
    session_agent_result(SessionId, Result),
    retract(session_agent_result(SessionId, _)),
    Result.success == true,
    % Use full messages from agent loop result
    (   get_dict(messages, Result, Messages), Messages \= []
    ->  % Replace memory with all messages from agent (includes system, history, and new messages)
        set_memory(StateIn, Messages, StateOut)
    ;   % Fallback: just add task description and response (old behavior)
        add_memory(StateIn, user, InterpDesc, State1),
        (   get_dict(response, Result, Response), Response \= ""
        ->  add_trace_with_result(SessionId, exit, task, [InterpDesc], Response, Depth),
            add_memory(State1, assistant, Response, StateOut)
        ;   add_trace_entry(SessionId, exit, task, [InterpDesc], Depth),
            StateOut = State1
        )
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

%% ============================================================
%% Prompt Handling - Fresh LLM call without existing memory
%% ============================================================

%% mi_call(prompt(Desc), +StateIn, -StateOut)
%% Like task/1 but starts with empty memory
mi_call(prompt(Desc), StateIn, StateIn) :-
    !,
    get_params(StateIn, Params),
    interpolate_desc(Desc, Params, InterpDesc),
    get_session_id(SessionId),
    collect_user_tools(SessionId, UserTools),
    % Use empty memory instead of current memory
    engine_yield(request_agent_loop(InterpDesc, [], UserTools, [])),
    engine_fetch(_Signal),
    session_agent_result(SessionId, Result),
    retract(session_agent_result(SessionId, _)),
    Result.success == true.
    % Don't modify memory - state unchanged

%% mi_call(prompt(Desc, Var1), +StateIn, -StateOut)
mi_call(prompt(Desc, Var1), StateIn, StateOut) :-
    !,
    mi_call_prompt_n(Desc, [Var1], ['Var1'], StateIn, StateOut).

%% mi_call(prompt(Desc, Var1, Var2), +StateIn, -StateOut)
mi_call(prompt(Desc, Var1, Var2), StateIn, StateOut) :-
    !,
    mi_call_prompt_n(Desc, [Var1, Var2], ['Var1', 'Var2'], StateIn, StateOut).

%% mi_call(prompt(Desc, Var1, Var2, Var3), +StateIn, -StateOut)
mi_call(prompt(Desc, Var1, Var2, Var3), StateIn, StateOut) :-
    !,
    mi_call_prompt_n(Desc, [Var1, Var2, Var3], ['Var1', 'Var2', 'Var3'], StateIn, StateOut).

%% mi_call_prompt_n(+Desc, +Vars, +VarNames, +StateIn, -StateOut)
%% Like mi_call_task_n but uses empty memory and doesn't update memory
mi_call_prompt_n(Desc, Vars, VarNames, StateIn, StateIn) :-
    get_params(StateIn, Params),
    interpolate_desc(Desc, Params, InterpDesc),
    get_session_id(SessionId),
    collect_user_tools(SessionId, UserTools),
    % Use empty memory instead of current memory
    engine_yield(request_agent_loop(InterpDesc, VarNames, UserTools, [])),
    engine_fetch(_Signal),
    session_agent_result(SessionId, Result),
    retract(session_agent_result(SessionId, _)),
    Result.success == true,
    bind_task_variables(Result.variables, VarNames, Vars).
    % Don't modify memory - state unchanged

%% mi_call_task_n(+Desc, +Vars, +VarNames, +StateIn, -StateOut)
mi_call_task_n(Desc, Vars, VarNames, StateIn, StateOut) :-
    get_params(StateIn, Params),
    interpolate_desc(Desc, Params, InterpDesc),
    get_session_id(SessionId),
    collect_user_tools(SessionId, UserTools),
    get_memory(StateIn, Memory),
    engine_yield(request_agent_loop(InterpDesc, VarNames, UserTools, Memory)),
    engine_fetch(_Signal),
    session_agent_result(SessionId, Result),
    retract(session_agent_result(SessionId, _)),
    Result.success == true,
    bind_task_variables(Result.variables, VarNames, Vars),
    % Use full messages from agent loop result
    (   get_dict(messages, Result, Messages), Messages \= []
    ->  % Replace memory with all messages from agent
        set_memory(StateIn, Messages, StateOut)
    ;   % Fallback: just add task description and response (old behavior)
        add_memory(StateIn, user, InterpDesc, State1),
        (   get_dict(response, Result, Response), Response \= ""
        ->  add_memory(State1, assistant, Response, StateOut)
        ;   StateOut = State1
        )
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
    get_depth(StateIn, Depth),
    add_trace_entry(SessionId, exec, ToolName, Args, Depth),
    engine_yield(request_exec(ToolName, Args)),
    engine_fetch(_Signal),
    session_exec_result(SessionId, Result),
    retract(session_exec_result(SessionId, _)),
    (   Result.status == success
    ->  Output = Result.result,
        add_trace_with_result(SessionId, exit, ToolName, Args, Output, Depth)
    ;   add_trace_entry(SessionId, fail, ToolName, Args, Depth),
        fail
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
%% answer/1 commits by throwing - prevents backtracking to other clauses
mi_call(answer(Text), StateIn, StateIn) :-
    get_params(StateIn, Params),
    interpolate_desc(Text, Params, InterpText),
    engine_yield(answer(InterpText)),
    throw('$answer_commit').

%% mi_call(output(Text), +StateIn, -StateOut)
mi_call(output(Text), StateIn, StateIn) :-
    get_params(StateIn, Params),
    interpolate_desc(Text, Params, InterpText),
    get_session_id(SessionId),
    get_depth(StateIn, Depth),
    add_trace_entry(SessionId, output, output, [InterpText], Depth),
    engine_yield(output(InterpText)).

%% mi_call(input(Prompt, Input), +StateIn, -StateOut)
%% Request input from the user with a prompt
mi_call(input(Prompt, Input), StateIn, StateIn) :-
    get_params(StateIn, Params),
    interpolate_desc(Prompt, Params, InterpPrompt),
    get_session_id(SessionId),
    get_depth(StateIn, Depth),
    add_trace_entry(SessionId, input, input, [InterpPrompt], Depth),
    engine_yield(wait_input(InterpPrompt)),
    engine_fetch(_Signal),
    % Retrieve the input provided via provide_input/2
    (   session_pending_input(SessionId, Input)
    ->  retract(session_pending_input(SessionId, Input)),
        add_trace_with_result(SessionId, exit, input, [InterpPrompt], Input, Depth)
    ;   Input = "",  % Default to empty if no input provided
        add_trace_with_result(SessionId, exit, input, [InterpPrompt], "", Depth)
    ).

%% mi_call(yield(Text), +StateIn, -StateOut)
mi_call(yield(Text), StateIn, StateIn) :-
    get_params(StateIn, Params),
    interpolate_desc(Text, Params, InterpText),
    get_session_id(SessionId),
    get_depth(StateIn, Depth),
    add_trace_entry(SessionId, output, yield, [InterpText], Depth),
    engine_yield(output(InterpText)).

%% mi_call(log(Text), +StateIn, -StateOut)
mi_call(log(Text), StateIn, StateIn) :-
    get_params(StateIn, Params),
    interpolate_desc(Text, Params, InterpText),
    engine_yield(log(InterpText)).

%% ============================================================
%% Context Stack Management - for memory save/restore
%% ============================================================

%% mi_call(push_context, +StateIn, -StateOut)
%% Save current memory to stack, keep memory active
mi_call(push_context, StateIn, StateOut) :-
    !,
    get_memory(StateIn, Memory),
    get_context_stack(StateIn, Stack),
    set_context_stack(StateIn, [Memory|Stack], StateOut).

%% mi_call(push_context(clear), +StateIn, -StateOut)
%% Save current memory to stack AND clear memory
mi_call(push_context(clear), StateIn, StateOut) :-
    !,
    get_memory(StateIn, Memory),
    get_context_stack(StateIn, Stack),
    set_context_stack(StateIn, [Memory|Stack], State1),
    set_memory(State1, [], StateOut).

%% mi_call(pop_context, +StateIn, -StateOut)
%% Restore memory from stack
mi_call(pop_context, StateIn, StateOut) :-
    !,
    get_context_stack(StateIn, Stack),
    (   Stack = [SavedMemory|RestStack]
    ->  set_memory(StateIn, SavedMemory, State1),
        set_context_stack(State1, RestStack, StateOut)
    ;   % Empty stack - do nothing
        StateOut = StateIn
    ).

%% mi_call(clear_memory, +StateIn, -StateOut)
%% Clear current memory without saving
mi_call(clear_memory, StateIn, StateOut) :-
    !,
    set_memory(StateIn, [], StateOut).

%% ============================================================
%% Parameter Handling
%% ============================================================

%% mi_call(param(Key, Value), +StateIn, -StateOut)
%% Simple param/2 lookup from session module (asserted facts)
mi_call(param(Key, Value), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    (   atom(Key)
    ->  KeyAtom = Key
    ;   atom_string(KeyAtom, Key)
    ),
    SessionId:param(KeyAtom, Value).

%% mi_call(param(Key, Desc, Value), +StateIn, -StateOut)
%% Legacy param/3 - still queries from state params dict
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
%% List Predicates - Interpreted for proper state threading
%% ============================================================

%% mi_call(member(X, List), +StateIn, -StateOut)
%% List membership - allows backtracking through list elements
mi_call(member(X, List), StateIn, StateIn) :-
    member(X, List).

%% mi_call(append(A, B, C), +StateIn, -StateOut)
mi_call(append(A, B, C), StateIn, StateIn) :-
    append(A, B, C).

%% mi_call(length(List, Len), +StateIn, -StateOut)
mi_call(length(List, Len), StateIn, StateIn) :-
    length(List, Len).

%% mi_call(nth0(Index, List, Elem), +StateIn, -StateOut)
mi_call(nth0(Index, List, Elem), StateIn, StateIn) :-
    nth0(Index, List, Elem).

%% mi_call(nth1(Index, List, Elem), +StateIn, -StateOut)
mi_call(nth1(Index, List, Elem), StateIn, StateIn) :-
    nth1(Index, List, Elem).

%% mi_call(last(List, Elem), +StateIn, -StateOut)
mi_call(last(List, Elem), StateIn, StateIn) :-
    last(List, Elem).

%% mi_call(reverse(List, Reversed), +StateIn, -StateOut)
mi_call(reverse(List, Reversed), StateIn, StateIn) :-
    reverse(List, Reversed).

%% mi_call(sort(List, Sorted), +StateIn, -StateOut)
mi_call(sort(List, Sorted), StateIn, StateIn) :-
    sort(List, Sorted).

%% mi_call(msort(List, Sorted), +StateIn, -StateOut)
mi_call(msort(List, Sorted), StateIn, StateIn) :-
    msort(List, Sorted).

%% ============================================================
%% Meta-Predicates - Interpreted with proper state threading
%% ============================================================

%% mi_call(include(Goal, List, Included), +StateIn, -StateOut)
%% Filter list keeping elements where Goal succeeds
mi_call(include(Goal, List, Included), StateIn, StateOut) :-
    mi_include(Goal, List, Included, StateIn, StateOut).

mi_include(_, [], [], State, State).
mi_include(Goal, [H|T], Result, StateIn, StateOut) :-
    copy_term(Goal, GoalCopy),
    GoalCopy =.. GoalList,
    append(GoalList, [H], GoalWithArg),
    TestGoal =.. GoalWithArg,
    (   mi_call(TestGoal, StateIn, State1)
    ->  Result = [H|Rest],
        mi_include(Goal, T, Rest, State1, StateOut)
    ;   mi_include(Goal, T, Result, StateIn, StateOut)
    ).

%% mi_call(exclude(Goal, List, Excluded), +StateIn, -StateOut)
%% Filter list removing elements where Goal succeeds
mi_call(exclude(Goal, List, Excluded), StateIn, StateOut) :-
    mi_exclude(Goal, List, Excluded, StateIn, StateOut).

mi_exclude(_, [], [], State, State).
mi_exclude(Goal, [H|T], Result, StateIn, StateOut) :-
    copy_term(Goal, GoalCopy),
    GoalCopy =.. GoalList,
    append(GoalList, [H], GoalWithArg),
    TestGoal =.. GoalWithArg,
    (   mi_call(TestGoal, StateIn, State1)
    ->  mi_exclude(Goal, T, Result, State1, StateOut)
    ;   Result = [H|Rest],
        mi_exclude(Goal, T, Rest, StateIn, StateOut)
    ).

%% mi_call(partition(Goal, List, Included, Excluded), +StateIn, -StateOut)
%% Partition list into elements where Goal succeeds and fails
mi_call(partition(Goal, List, Included, Excluded), StateIn, StateOut) :-
    mi_partition(Goal, List, Included, Excluded, StateIn, StateOut).

mi_partition(_, [], [], [], State, State).
mi_partition(Goal, [H|T], Inc, Exc, StateIn, StateOut) :-
    copy_term(Goal, GoalCopy),
    GoalCopy =.. GoalList,
    append(GoalList, [H], GoalWithArg),
    TestGoal =.. GoalWithArg,
    (   mi_call(TestGoal, StateIn, State1)
    ->  Inc = [H|IncRest],
        mi_partition(Goal, T, IncRest, Exc, State1, StateOut)
    ;   Exc = [H|ExcRest],
        mi_partition(Goal, T, Inc, ExcRest, StateIn, StateOut)
    ).

%% mi_call(forall(Cond, Action), +StateIn, -StateOut)
%% Succeed if for all solutions of Cond, Action succeeds
%% Note: State changes in Action are NOT preserved (bagof semantics)
mi_call(forall(Cond, Action), StateIn, StateIn) :-
    \+ (mi_call(Cond, StateIn, State1), \+ mi_call(Action, State1, _)).

%% mi_call(findall(Template, Goal, List), +StateIn, -StateOut)
%% Collect all solutions - state changes are NOT preserved
mi_call(findall(Template, Goal, List), StateIn, StateIn) :-
    findall(Template, mi_call(Goal, StateIn, _), List).

%% mi_call(bagof(Template, Goal, List), +StateIn, -StateOut)
mi_call(bagof(Template, Goal, List), StateIn, StateIn) :-
    bagof(Template, mi_call(Goal, StateIn, _), List).

%% mi_call(setof(Template, Goal, List), +StateIn, -StateOut)
mi_call(setof(Template, Goal, List), StateIn, StateIn) :-
    setof(Template, mi_call(Goal, StateIn, _), List).

%% mi_call(aggregate_all(Template, Goal, Result), +StateIn, -StateOut)
mi_call(aggregate_all(Template, Goal, Result), StateIn, StateIn) :-
    aggregate_all(Template, mi_call(Goal, StateIn, _), Result).

%% mi_call(maplist(Goal, List), +StateIn, -StateOut)
%% Apply Goal to each element (Goal/1)
mi_call(maplist(Goal, List), StateIn, StateOut) :-
    mi_maplist1(Goal, List, StateIn, StateOut).

mi_maplist1(_, [], State, State).
mi_maplist1(Goal, [H|T], StateIn, StateOut) :-
    copy_term(Goal, GoalCopy),
    GoalCopy =.. GoalList,
    append(GoalList, [H], GoalWithArg),
    CallGoal =.. GoalWithArg,
    mi_call(CallGoal, StateIn, State1),
    mi_maplist1(Goal, T, State1, StateOut).

%% mi_call(maplist(Goal, List1, List2), +StateIn, -StateOut)
%% Apply Goal to pairs of elements (Goal/2)
mi_call(maplist(Goal, List1, List2), StateIn, StateOut) :-
    mi_maplist2(Goal, List1, List2, StateIn, StateOut).

mi_maplist2(_, [], [], State, State).
mi_maplist2(Goal, [H1|T1], [H2|T2], StateIn, StateOut) :-
    copy_term(Goal, GoalCopy),
    GoalCopy =.. GoalList,
    append(GoalList, [H1, H2], GoalWithArgs),
    CallGoal =.. GoalWithArgs,
    mi_call(CallGoal, StateIn, State1),
    mi_maplist2(Goal, T1, T2, State1, StateOut).

%% mi_call(maplist(Goal, L1, L2, L3), +StateIn, -StateOut)
%% Apply Goal to triples of elements (Goal/3)
mi_call(maplist(Goal, L1, L2, L3), StateIn, StateOut) :-
    mi_maplist3(Goal, L1, L2, L3, StateIn, StateOut).

mi_maplist3(_, [], [], [], State, State).
mi_maplist3(Goal, [H1|T1], [H2|T2], [H3|T3], StateIn, StateOut) :-
    copy_term(Goal, GoalCopy),
    GoalCopy =.. GoalList,
    append(GoalList, [H1, H2, H3], GoalWithArgs),
    CallGoal =.. GoalWithArgs,
    mi_call(CallGoal, StateIn, State1),
    mi_maplist3(Goal, T1, T2, T3, State1, StateOut).

%% mi_call(foldl(Goal, List, V0, V), +StateIn, -StateOut)
%% Left fold over list
mi_call(foldl(Goal, List, V0, V), StateIn, StateOut) :-
    mi_foldl(Goal, List, V0, V, StateIn, StateOut).

mi_foldl(_, [], V, V, State, State).
mi_foldl(Goal, [H|T], V0, V, StateIn, StateOut) :-
    copy_term(Goal, GoalCopy),
    GoalCopy =.. GoalList,
    append(GoalList, [H, V0, V1], GoalWithArgs),
    CallGoal =.. GoalWithArgs,
    mi_call(CallGoal, StateIn, State1),
    mi_foldl(Goal, T, V1, V, State1, StateOut).

%% ============================================================
%% File I/O - All paths relative to /workspace
%% ============================================================

%% Helper: resolve path relative to /workspace
%% Prevents directory traversal outside workspace
resolve_workspace_path(RelPath, FullPath) :-
    (   atom(RelPath) -> atom_string(RelPath, RelPathStr)
    ;   RelPathStr = RelPath
    ),
    % Remove leading slashes to make relative
    (   sub_string(RelPathStr, 0, 1, _, "/")
    ->  sub_string(RelPathStr, 1, _, 0, CleanPath)
    ;   CleanPath = RelPathStr
    ),
    % Prevent directory traversal
    (   sub_string(CleanPath, _, _, _, "..")
    ->  throw(error(permission_error(access, directory, RelPath), 
                    context(resolve_workspace_path/2, 'Directory traversal not allowed')))
    ;   true
    ),
    % Build full path
    atom_concat('/workspace/', CleanPath, FullPath).

%% mi_call(read_file_to_string(File, Content, Options), +StateIn, -StateOut)
mi_call(read_file_to_string(File, Content, Options), StateIn, StateIn) :-
    !,
    resolve_workspace_path(File, FullPath),
    readutil:read_file_to_string(FullPath, Content, Options).

%% mi_call(read_file(File, Content), +StateIn, -StateOut)
%% Simplified version without options
mi_call(read_file(File, Content), StateIn, StateIn) :-
    !,
    resolve_workspace_path(File, FullPath),
    readutil:read_file_to_string(FullPath, Content, []).

%% mi_call(write_file(File, Content), +StateIn, -StateOut)
%% Write string content to a file
mi_call(write_file(File, Content), StateIn, StateIn) :-
    !,
    resolve_workspace_path(File, FullPath),
    open(FullPath, write, Stream),
    write(Stream, Content),
    close(Stream).

%% mi_call(append_file(File, Content), +StateIn, -StateOut)
%% Append string content to a file
mi_call(append_file(File, Content), StateIn, StateIn) :-
    !,
    resolve_workspace_path(File, FullPath),
    open(FullPath, append, Stream),
    write(Stream, Content),
    close(Stream).

%% mi_call(open(File, Mode, Stream), +StateIn, -StateOut)
%% Open a file for reading/writing
mi_call(open(File, Mode, Stream), StateIn, StateIn) :-
    !,
    resolve_workspace_path(File, FullPath),
    open(FullPath, Mode, Stream).

%% mi_call(close(Stream), +StateIn, -StateOut)
mi_call(close(Stream), StateIn, StateIn) :-
    !,
    close(Stream).

%% mi_call(read_string(Stream, Length, Content), +StateIn, -StateOut)
mi_call(read_string(Stream, Length, Content), StateIn, StateIn) :-
    !,
    read_string(Stream, Length, Content).

%% mi_call(write(Stream, Content), +StateIn, -StateOut)
mi_call(write(Stream, Content), StateIn, StateIn) :-
    !,
    write(Stream, Content).

%% mi_call(exists_file(File), +StateIn, -StateOut)
mi_call(exists_file(File), StateIn, StateIn) :-
    !,
    resolve_workspace_path(File, FullPath),
    exists_file(FullPath).

%% mi_call(exists_directory(Dir), +StateIn, -StateOut)
mi_call(exists_directory(Dir), StateIn, StateIn) :-
    !,
    resolve_workspace_path(Dir, FullPath),
    exists_directory(FullPath).

%% mi_call(directory_files(Dir, Files), +StateIn, -StateOut)
mi_call(directory_files(Dir, Files), StateIn, StateIn) :-
    !,
    resolve_workspace_path(Dir, FullPath),
    directory_files(FullPath, Files).

%% mi_call(make_directory(Dir), +StateIn, -StateOut)
mi_call(make_directory(Dir), StateIn, StateIn) :-
    !,
    resolve_workspace_path(Dir, FullPath),
    make_directory(FullPath).

%% mi_call(delete_file(File), +StateIn, -StateOut)
mi_call(delete_file(File), StateIn, StateIn) :-
    !,
    resolve_workspace_path(File, FullPath),
    delete_file(FullPath).

%% mi_call(delete_directory(Dir), +StateIn, -StateOut)
mi_call(delete_directory(Dir), StateIn, StateIn) :-
    !,
    resolve_workspace_path(Dir, FullPath),
    delete_directory(FullPath).

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
    predicate_property(Module:Goal, defined),
    !,  % CUT: If predicate is defined, don't fallback to call/1
    % Now try clause/2 for rules, allowing backtracking through clauses
    clause(Module:Goal, Body),
    (   Body == true
    ->  true  % Fact - succeed with no body to interpret
    ;   mi_call(Body, StateIn, StateOut)
    ).

mi_call(Module:Goal, StateIn, StateIn) :-
    % Fallback for built-ins only (predicate is NOT defined in module)
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
is_mi_special_predicate(param(_,_)).
is_mi_special_predicate(param(_,_,_)).
%% Prompt predicates (like task but with fresh memory)
is_mi_special_predicate(prompt(_)).
is_mi_special_predicate(prompt(_,_)).
is_mi_special_predicate(prompt(_,_,_)).
is_mi_special_predicate(prompt(_,_,_,_)).
%% Context stack management
is_mi_special_predicate(push_context).
is_mi_special_predicate(push_context(_)).
is_mi_special_predicate(pop_context).
is_mi_special_predicate(clear_memory).
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
%% List predicates
is_mi_special_predicate(member(_,_)).
is_mi_special_predicate(append(_,_,_)).
is_mi_special_predicate(length(_,_)).
is_mi_special_predicate(nth0(_,_,_)).
is_mi_special_predicate(nth1(_,_,_)).
is_mi_special_predicate(last(_,_)).
is_mi_special_predicate(reverse(_,_)).
is_mi_special_predicate(sort(_,_)).
is_mi_special_predicate(msort(_,_)).
%% Meta-predicates
is_mi_special_predicate(include(_,_,_)).
is_mi_special_predicate(exclude(_,_,_)).
is_mi_special_predicate(partition(_,_,_,_)).
is_mi_special_predicate(forall(_,_)).
is_mi_special_predicate(findall(_,_,_)).
is_mi_special_predicate(bagof(_,_,_)).
is_mi_special_predicate(setof(_,_,_)).
is_mi_special_predicate(aggregate_all(_,_,_)).
is_mi_special_predicate(maplist(_,_)).
is_mi_special_predicate(maplist(_,_,_)).
is_mi_special_predicate(maplist(_,_,_,_)).
is_mi_special_predicate(foldl(_,_,_,_)).
%% File I/O predicates
is_mi_special_predicate(read_file_to_string(_,_,_)).
is_mi_special_predicate(read_file(_,_)).
is_mi_special_predicate(write_file(_,_)).
is_mi_special_predicate(append_file(_,_)).
is_mi_special_predicate(open(_,_,_)).
is_mi_special_predicate(close(_)).
is_mi_special_predicate(read_string(_,_,_)).
is_mi_special_predicate(write(_,_)).
is_mi_special_predicate(exists_file(_)).
is_mi_special_predicate(exists_directory(_)).
is_mi_special_predicate(directory_files(_,_)).
is_mi_special_predicate(make_directory(_)).
is_mi_special_predicate(delete_file(_)).
is_mi_special_predicate(delete_directory(_)).

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
    % Add trace entry for call
    get_depth(StateIn, Depth),
    Goal =.. [Functor|Args],
    add_trace_entry(SessionId, call, Functor, Args, Depth),
    % NO CUT HERE! This allows backtracking through clause/2
    clause(SessionId:Goal, Body),
    NewDepth is Depth + 1,
    set_depth(StateIn, NewDepth, State1),
    (   mi_call(Body, State1, State2)
    ->  add_trace_entry(SessionId, exit, Functor, Args, Depth),
        set_depth(State2, Depth, StateOut)
    ;   add_trace_entry(SessionId, fail, Functor, Args, Depth),
        fail
    ).

mi_call_dispatch(Goal, StateIn, StateIn) :-
    % Fallback for external/library predicates
    get_session_id(SessionId),
    % Check if this predicate exists in the session module but not as user-defined
    \+ predicate_property(SessionId:Goal, defined),
    catch(call(SessionId:Goal), _, fail),
    !.

mi_call_dispatch(Goal, StateIn, StateIn) :-
    % Final fallback: call without module
    % Only for predicates not defined in session
    get_session_id(SessionId),
    \+ predicate_property(SessionId:Goal, defined),
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

%% ============================================================
%% Compile-Time String Interpolation Expansion
%% ============================================================

%% expand_interpolations(+Term, +Bindings, -ExpandedTerm)
%% Walk a term and expand strings containing {VarName} patterns
%% Bindings is a list of 'VarName'=Variable pairs from read_term
expand_interpolations(Term, Bindings, ExpandedTerm) :-
    expand_term_interp(Term, Bindings, ExpandedTerm).

%% expand_term_interp(+Term, +Bindings, -Expanded)
%% Main term expansion predicate

% Variables pass through unchanged
expand_term_interp(Var, _, Var) :-
    var(Var), !.

% Strings - check if they need interpolation
expand_term_interp(String, Bindings, Expanded) :-
    string(String), !,
    (   string_needs_interpolation(String)
    ->  build_format_goal(String, Bindings, Expanded)
    ;   Expanded = String
    ).

% Atoms - check if they need interpolation (for atom strings)
expand_term_interp(Atom, Bindings, Expanded) :-
    atom(Atom),
    \+ Atom = [],  % Not empty list
    atom_string(Atom, String),
    string_needs_interpolation(String), !,
    build_format_goal(String, Bindings, ExpandedGoal),
    % Wrap in atom conversion if original was atom
    Expanded = ExpandedGoal.

% Regular atoms pass through
expand_term_interp(Atom, _, Atom) :-
    atom(Atom), !.

% Numbers pass through
expand_term_interp(Num, _, Num) :-
    number(Num), !.

% Empty list
expand_term_interp([], _, []) :- !.

% Lists - expand each element
expand_term_interp([H|T], Bindings, [EH|ET]) :- !,
    expand_term_interp(H, Bindings, EH),
    expand_term_interp(T, Bindings, ET).

% Clause with body - special handling for string interpolation in goals
expand_term_interp((Head :- Body), Bindings, (Head :- ExpandedBody)) :- !,
    expand_body_interp(Body, Bindings, ExpandedBody).

% Other compound terms - expand arguments
expand_term_interp(Term, Bindings, ExpandedTerm) :-
    compound(Term), !,
    Term =.. [Functor|Args],
    maplist({Bindings}/[A, EA]>>expand_term_interp(A, Bindings, EA), Args, ExpandedArgs),
    ExpandedTerm =.. [Functor|ExpandedArgs].

%% expand_body_interp(+Body, +Bindings, -ExpandedBody)
%% Expand goals in a clause body, handling control structures

% Variable goal
expand_body_interp(Var, _, Var) :-
    var(Var), !.

% Conjunction
expand_body_interp((A, B), Bindings, ExpandedConj) :- !,
    expand_body_interp(A, Bindings, EA),
    expand_body_interp(B, Bindings, EB),
    flatten_conjunction(EA, EB, ExpandedConj).

% Disjunction  
expand_body_interp((A ; B), Bindings, (EA ; EB)) :- !,
    expand_body_interp(A, Bindings, EA),
    expand_body_interp(B, Bindings, EB).

% If-then-else
expand_body_interp((A -> B ; C), Bindings, (EA -> EB ; EC)) :- !,
    expand_body_interp(A, Bindings, EA),
    expand_body_interp(B, Bindings, EB),
    expand_body_interp(C, Bindings, EC).

% If-then
expand_body_interp((A -> B), Bindings, (EA -> EB)) :- !,
    expand_body_interp(A, Bindings, EA),
    expand_body_interp(B, Bindings, EB).

% Soft cut
expand_body_interp((A *-> B), Bindings, (EA *-> EB)) :- !,
    expand_body_interp(A, Bindings, EA),
    expand_body_interp(B, Bindings, EB).

% Negation
expand_body_interp(\+(A), Bindings, \+(EA)) :- !,
    expand_body_interp(A, Bindings, EA).

% Goals that take string arguments - expand the string arg specially
expand_body_interp(Goal, Bindings, ExpandedGoal) :-
    goal_with_string_arg(Goal, Functor, StringArg, RestArgs), !,
    (   string_needs_interpolation(StringArg)
    ->  build_format_call(StringArg, Bindings, TempVar, FormatGoal),
        rebuild_goal(Functor, TempVar, RestArgs, NewGoal),
        ExpandedGoal = (FormatGoal, NewGoal)
    ;   expand_rest_args(RestArgs, Bindings, ExpandedRestArgs),
        rebuild_goal(Functor, StringArg, ExpandedRestArgs, ExpandedGoal)
    ).

% Other goals - expand arguments
expand_body_interp(Goal, Bindings, ExpandedGoal) :-
    compound(Goal), !,
    Goal =.. [Functor|Args],
    maplist({Bindings}/[A, EA]>>expand_body_interp(A, Bindings, EA), Args, ExpandedArgs),
    ExpandedGoal =.. [Functor|ExpandedArgs].

% Atoms/other
expand_body_interp(Goal, _, Goal).

%% goal_with_string_arg(+Goal, -Functor, -StringArg, -RestArgs)
%% Match goals that take a string as their first argument
goal_with_string_arg(answer(S), answer, S, []) :- string(S).
goal_with_string_arg(answer(S), answer, S, []) :- atom(S), \+ S = [].
goal_with_string_arg(system(S), system, S, []) :- string(S).
goal_with_string_arg(system(S), system, S, []) :- atom(S), \+ S = [].
goal_with_string_arg(user(S), user, S, []) :- string(S).
goal_with_string_arg(user(S), user, S, []) :- atom(S), \+ S = [].
goal_with_string_arg(output(S), output, S, []) :- string(S).
goal_with_string_arg(output(S), output, S, []) :- atom(S), \+ S = [].
goal_with_string_arg(log(S), log, S, []) :- string(S).
goal_with_string_arg(log(S), log, S, []) :- atom(S), \+ S = [].
goal_with_string_arg(yield(S), yield, S, []) :- string(S).
goal_with_string_arg(yield(S), yield, S, []) :- atom(S), \+ S = [].
goal_with_string_arg(task(S), task, S, []) :- string(S).
goal_with_string_arg(task(S), task, S, []) :- atom(S), \+ S = [].
goal_with_string_arg(task(S, V1), task, S, [V1]) :- string(S).
goal_with_string_arg(task(S, V1), task, S, [V1]) :- atom(S), \+ S = [].
goal_with_string_arg(task(S, V1, V2), task, S, [V1, V2]) :- string(S).
goal_with_string_arg(task(S, V1, V2), task, S, [V1, V2]) :- atom(S), \+ S = [].
goal_with_string_arg(task(S, V1, V2, V3), task, S, [V1, V2, V3]) :- string(S).
goal_with_string_arg(task(S, V1, V2, V3), task, S, [V1, V2, V3]) :- atom(S), \+ S = [].
goal_with_string_arg(prompt(S), prompt, S, []) :- string(S).
goal_with_string_arg(prompt(S), prompt, S, []) :- atom(S), \+ S = [].
goal_with_string_arg(prompt(S, V1), prompt, S, [V1]) :- string(S).
goal_with_string_arg(prompt(S, V1), prompt, S, [V1]) :- atom(S), \+ S = [].
goal_with_string_arg(prompt(S, V1, V2), prompt, S, [V1, V2]) :- string(S).
goal_with_string_arg(prompt(S, V1, V2), prompt, S, [V1, V2]) :- atom(S), \+ S = [].
goal_with_string_arg(prompt(S, V1, V2, V3), prompt, S, [V1, V2, V3]) :- string(S).
goal_with_string_arg(prompt(S, V1, V2, V3), prompt, S, [V1, V2, V3]) :- atom(S), \+ S = [].

%% rebuild_goal(+Functor, +StringArg, +RestArgs, -Goal)
%% Rebuild a goal with the string argument and rest args
rebuild_goal(Functor, StringArg, [], Goal) :- !,
    Goal =.. [Functor, StringArg].
rebuild_goal(Functor, StringArg, RestArgs, Goal) :-
    Goal =.. [Functor, StringArg | RestArgs].

%% expand_rest_args(+Args, +Bindings, -ExpandedArgs)
expand_rest_args([], _, []).
expand_rest_args([A|As], Bindings, [EA|EAs]) :-
    expand_body_interp(A, Bindings, EA),
    expand_rest_args(As, Bindings, EAs).

%% flatten_conjunction(+A, +B, -Conj)
%% Flatten nested conjunctions properly
flatten_conjunction((A1, A2), B, Result) :- !,
    flatten_conjunction(A2, B, Rest),
    Result = (A1, Rest).
flatten_conjunction(A, B, (A, B)).

%% string_needs_interpolation(+String)
%% Check if a string contains {VarName} patterns
string_needs_interpolation(String) :-
    (   string(String) -> S = String
    ;   atom(String) -> atom_string(String, S)
    ;   fail
    ),
    sub_string(S, _, _, _, "{"),
    sub_string(S, _, _, _, "}").

%% build_format_call(+Template, +Bindings, -TempVar, -FormatGoal)
%% Build a format/3 call that produces the interpolated string
%% For variables in scope: use the variable directly
%% For params (not in scope): generate param(name, Var) lookup
build_format_call(Template, Bindings, TempVar, FullGoal) :-
    (   string(Template) -> T = Template
    ;   atom_string(Template, T)
    ),
    extract_interpolation_vars(T, VarNames, FormatString),
    lookup_vars_with_params(VarNames, Bindings, VarList, ParamGoals),
    FormatGoal = format(string(TempVar), FormatString, VarList),
    (   ParamGoals == []
    ->  FullGoal = FormatGoal
    ;   list_to_conjunction(ParamGoals, ParamConj),
        FullGoal = (ParamConj, FormatGoal)
    ).

%% list_to_conjunction(+List, -Conjunction)
list_to_conjunction([G], G) :- !.
list_to_conjunction([G|Gs], (G, Rest)) :-
    list_to_conjunction(Gs, Rest).

%% build_format_goal(+String, +Bindings, -Goal)
%% For direct string expansion (not in a goal context)
build_format_goal(String, Bindings, Goal) :-
    build_format_call(String, Bindings, TempVar, FormatGoal),
    Goal = (FormatGoal, TempVar).

%% extract_interpolation_vars(+Template, -VarNames, -FormatString)
%% Parse template to extract variable names and build format string
extract_interpolation_vars(Template, VarNames, FormatString) :-
    string_codes(Template, Codes),
    extract_vars_from_codes(Codes, VarNames, FormatCodes),
    string_codes(FormatString, FormatCodes).

%% extract_vars_from_codes(+Codes, -VarNames, -FormatCodes)
extract_vars_from_codes([], [], []) :- !.

% Found opening brace
extract_vars_from_codes([0'{|Rest], [VarName|VarNames], [0'~, 0'w|FormatRest]) :- !,
    extract_var_until_close(Rest, VarNameCodes, AfterClose),
    atom_codes(VarName, VarNameCodes),
    extract_vars_from_codes(AfterClose, VarNames, FormatRest).

% Escape tilde for format/3
extract_vars_from_codes([0'~|Rest], VarNames, [0'~, 0'~|FormatRest]) :- !,
    extract_vars_from_codes(Rest, VarNames, FormatRest).

% Regular character
extract_vars_from_codes([C|Rest], VarNames, [C|FormatRest]) :-
    extract_vars_from_codes(Rest, VarNames, FormatRest).

%% extract_var_until_close(+Codes, -VarNameCodes, -Rest)
extract_var_until_close([0'}|Rest], [], Rest) :- !.
extract_var_until_close([C|Rest], [C|VarRest], Final) :-
    extract_var_until_close(Rest, VarRest, Final).
extract_var_until_close([], [], []).

%% lookup_vars_with_params(+VarNames, +Bindings, -VarList, -ParamGoals)
%% Look up each variable name:
%% - If in Bindings (local Prolog variable) → use directly
%% - If not in Bindings → generate param(name, Var) lookup
lookup_vars_with_params([], _, [], []).
lookup_vars_with_params([Name|Names], Bindings, [Var|Vars], ParamGoals) :-
    (   member(Name=Var, Bindings)
    ->  % Found as local variable
        ParamGoals = RestGoals
    ;   % Not a local variable - treat as param lookup
        % Convert to lowercase atom for param key
        downcase_atom(Name, LowerName),
        ParamGoals = [param(LowerName, Var)|RestGoals]
    ),
    lookup_vars_with_params(Names, Bindings, Vars, RestGoals).

