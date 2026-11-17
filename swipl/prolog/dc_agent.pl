%% Agent loop for task/N and prompt/N.
:- module(dc_agent, [run/1, run/2, run/3]).

:- use_module(library(lists)).
:- use_module(library(aggregate)).
:- use_module('../../src/prolog-src/deepclause_mi.pl').
:- use_module(dc_config).
:- use_module(dc_json).
:- use_module(dc_llm).
:- use_module(dc_tools).
:- use_module(dc_events).

%% run(+Payload)
run(Payload) :-
    run(Payload, Result),
    (   is_dict(Result), get_dict(session, Result, Session)
    ->  get_dict(success, Result, Success),
        get_dict(variables, Result, Variables),
        get_dict(messages, Result, Messages),
        get_dict(full_memory, Result, FullMemory),
        get_dict(error, Result, Error),
        deepclause_mi:post_agent_result(Session, Success, Variables, Messages, FullMemory, Error)
    ;   true
    ).

%% run(+Payload, -Result)
run(Payload, Result) :-
    (   is_dict(Payload), get_dict(session, Payload, Session)
    ->  run(Session, Payload, Result)
    ;   ( nb_current(current_session_id, Session0) -> Session = Session0 ; Session = default_session ),
        run(Session, Payload, Result)
    ).

%% run(+Session, +Payload, -Result)
run(Session, Payload, Result) :-
    (   is_dict(Payload), get_dict(taskDescription, Payload, Task0)
    ->  dc_json:as_text(Task0, Task)
    ;   Task = ""
    ),
    (   is_dict(Payload), get_dict(mode, Payload, Mode0)
    ->  dc_json:key_atom(Mode0, Mode)
    ;   Mode = task
    ),
    (   is_dict(Payload), get_dict(outputVars, Payload, OutputVars0), is_list(OutputVars0)
    ->  OutputVars = OutputVars0
    ;   OutputVars = []
    ),
    (   is_dict(Payload), get_dict(userTools, Payload, RawTools), is_list(RawTools)
    ->  RawTools2 = RawTools
    ;   RawTools2 = []
    ),
    (   is_dict(Payload), get_dict(toolScope, Payload, Scope)
    ->  true
    ;   Scope = none
    ),
    available_tools(RawTools2, Scope, Tools0),
    synthetic_tools(Mode, OutputVars, Tools0, Tools),
    (   is_dict(Payload), get_dict(memory, Payload, Memory0), is_list(Memory0)
    ->  Memory = Memory0
    ;   Memory = []
    ),
    system_prompt(Task, OutputVars, Tools, Mode, SystemPrompt),
    (   Mode == chat
    ->  UserMessage = Task
    ;   string_concat("Subtask: ", Task, UserMessage)
    ),
    initial_messages(SystemPrompt, Memory, UserMessage, Messages0),
    findall(Sys, (member(Msg, Memory), message_role(Msg, system), Msg = Sys), BaseSystem),
    State0 = agent_state{messages: Messages0, variables: _{}, finished: false,
                         success: false, calls: 0, nudges: 0, session: Session,
                         tools: Tools, tool_info: RawTools2, output_vars: OutputVars,
                         base_system: BaseSystem},
    catch(
        run_loop(State0, State),
        Error,
        (   format(string(Message), "agent loop failed: ~w", [Error]),
            dc_events:emit(error(Message)),
            State = State0.put(finished, true).put(success, false)
                .put(error, Message)
        )
    ),
    final_result(State, Task, Result).

final_result(State, Task, Result) :-
    get_dict(variables, State, Variables),
    get_dict(messages, State, Messages0),
    get_dict(success, State, Success0),
    (   Success0 == true -> Success = true ; Success = false ),
    (   get_dict(error, State, Error) -> true ; Error = "" ),
    keep_memory(Messages0, Conversation),
    (   get_dict(base_system, State, BaseSystem) -> true ; BaseSystem = [] ),
    append(BaseSystem, Conversation, MemoryMessages),
    (   get_dict(session, State, Session) -> true ; Session = none ),
    Result = result{session: Session, success: Success, variables: Variables,
                    messages: Conversation, full_memory: MemoryMessages,
                    error: Error, task: Task}.

run_loop(State0, State) :-
    get_dict(finished, State0, Finished),
    (   Finished == true
    ->  State = State0
    ;   get_dict(calls, State0, Calls),
        dc_config:max_model_calls(Max),
        (   Max > 0, Calls >= Max
        ->  State = State0.put(finished, true).put(success, false)
                .put(error, "model-call budget exhausted")
        ;   get_dict(messages, State0, Messages),
            (   get_dict(tools, State0, Tools) -> true ; Tools = [] ),
            dc_llm:request_from_messages(Messages, Tools, Request0),
            reasoning_of(State0, Reasoning),
            Request = Request0.put(reasoning, Reasoning),
            dc_events:emit(model_call(started)),
            catch(
                dc_llm:stream(Request, dc_agent:llm_event, Response),
                Error,
                (   format(string(Message), "model call failed: ~w", [Error]),
                    dc_events:emit(error(Message)),
                    dc_events:emit(model_call(completed)),
                    throw(Error)
                )
            ),
            dc_events:emit(model_call(completed)),
            get_dict(text, Response, Text),
            get_dict(tool_calls, Response, ToolCalls),
            Assistant = llm_message{role: assistant, content: Text, tool_calls: ToolCalls},
            append(Messages, [Assistant], Messages1),
            process_tool_calls(ToolCalls, State0.put(messages, Messages1), State1),
            get_dict(finished, State1, NowFinished),
            (   NowFinished == true
            ->  State = State1
            ;   maybe_nudge(State1, State2),
                run_loop(State2, State)
            )
        )
    ).

reasoning_of(State, Reasoning) :-
    ( get_dict(reasoning, State, R) -> Reasoning = R ; Reasoning = none ).

%% ---------------------------------------------------------------
%% Tools
%% ---------------------------------------------------------------

available_tools(Tools0, Scope, Tools) :-
    exclude(excluded_by_scope(Scope), Tools0, Scoped),
    maplist(tool_from_info, Scoped, Tools).

excluded_by_scope(whitelist(List), Tool) :-
    tool_info_name(Tool, Name),
    \+ memberchk(Name, List).
excluded_by_scope(blacklist(List), Tool) :-
    tool_info_name(Tool, Name),
    memberchk(Name, List).
excluded_by_scope(_, _) :- fail.

tool_info_name(ToolInfo, Name) :-
    is_dict(ToolInfo),
    get_dict(name, ToolInfo, Name0),
    dc_json:as_atom(Name0, Name).

tool_from_info(ToolInfo, llm_tool{name: Name, description: Description,
                                 parameters: Parameters, source: ToolInfo}) :-
    is_dict(ToolInfo),
    tool_info_name(ToolInfo, Name),
    dc_json:tool_description(ToolInfo, Description),
    dc_json:tool_json_schema(ToolInfo, Parameters).

synthetic_tools(chat, _, Tools, Tools) :- !.
synthetic_tools(_, OutputVars, Tools0, Tools) :-
    Finish = llm_tool{
        name: finish,
        description: "Complete the task. success=true requires every requested result variable to be set.",
        parameters: _{type: object, properties: _{success: _{type: boolean}},
                      required: [success]},
        source: none},
    (   OutputVars == []
    ->  Tools = [Finish|Tools0]
    ;   output_names(OutputVars, Names),
        Result = llm_tool{
            name: set_result,
            description: "Set one requested output variable before calling finish.",
            parameters: _{type: object,
                          properties: _{variable: _{type: string, enum: Names},
                                        value: _{}},
                          required: [variable, value]},
            source: none},
        Tools = [Finish, Result|Tools0]
    ).

output_names(OutputVars, Names) :-
    maplist(output_name, OutputVars, Names).

output_name(Var, Name) :-
    (   is_dict(Var), get_dict(name, Var, N)
    ->  dc_json:as_atom(N, Name)
    ;   dc_json:as_atom(Var, Name)
    ).

%% ---------------------------------------------------------------
%% Prompts
%% ---------------------------------------------------------------

system_prompt(Task, OutputVars, Tools, Mode, Prompt) :-
    tool_lines(Tools, ToolLines),
    (   Mode == chat
    ->  format(string(Prompt),
            "You are an AI agent in an ongoing conversation.~n~nTASK~n~w~n~nTOOLS~n~w~n~nCall tools when useful. When you have nothing left to do, reply in plain text and stop.~n",
            [Task, ToolLines])
    ;   output_lines(OutputVars, OutputLines),
        format(string(Prompt),
            "You are an AI agent executing a DeepClause subtask.~n~nSUBTASK~n~w~n~nTOOLS~n~w~n~nREQUIRED RESULTS~n~w~n~nUse the structured tool-calling interface. Call finish(true) when done; call finish(false) if the task cannot be completed.~n",
            [Task, ToolLines, OutputLines])
    ).

tool_lines([], "- none").
tool_lines([], "- none") :- !.
tool_lines(Tools, Text) :-
    findall(Line,
        (   member(Tool, Tools),
            get_dict(name, Tool, Name),
            get_dict(description, Tool, Description),
            get_dict(parameters, Tool, Parameters),
            parameter_line(Parameters, Params),
            format(string(Line), "- ~w(~w): ~w", [Name, Params, Description])
        ),
        Lines),
    ( Lines == [] -> Text = "- none" ; atomic_list_concat(Lines, "\n", Text) ).

parameter_line(Parameters, Params) :-
    (   is_dict(Parameters),
        get_dict(properties, Parameters, Props),
        is_dict(Props)
    ->  dict_pairs(Props, _, Pairs),
        findall(Text, (member(Key-_, Pairs), atomic_list_concat([Key], Text)), Ps),
        atomic_list_concat(Ps, ", ", Params)
    ;   Params = ""
    ).

output_lines([], "none").
output_lines(OutputVars, Text) :-
    findall(Line,
        ( member(Var, OutputVars),
          output_name(Var, Name),
          output_type(Var, Type),
          format(string(Line), "- ~w: ~w", [Name, Type])
        ),
        Lines),
    atomic_list_concat(Lines, "\n", Text).

output_type(Var, Type) :-
    (   is_dict(Var), get_dict(type, Var, Type0)
    ->  dc_json:as_text(Type0, Type)
    ;   Type = "string"
    ).

initial_messages(SystemPrompt, Memory, UserMessage, Messages) :-
    findall(M, (member(M, Memory), message_role(M, R), R \== system), Conversation),
    append([llm_message{role: system, content: SystemPrompt}], Conversation, Seed),
    append(Seed, [llm_message{role: user, content: UserMessage}], Messages).

message_role(Message, Role) :-
    ( is_dict(Message), get_dict(role, Message, R) -> dc_json:key_atom(R, Role) ; Role = user ).

%% ---------------------------------------------------------------
%% Tool calls
%% ---------------------------------------------------------------

process_tool_calls([], State, State).
process_tool_calls([Call|Calls], State0, State) :-
    call_field(Call, name, "", Name0),
    dc_json:as_atom(Name0, Name),
    call_field(Call, id, "", Id),
    call_arguments(Call, Arguments),
    (   Name == finish
    ->  process_finish(Id, Arguments, State0, State1)
    ;   Name == set_result
    ->  process_set_result(Id, Arguments, State0, State1)
    ;   process_user_tool(Name, Id, Arguments, State0, State1)
    ),
    get_dict(finished, State1, Finished),
    (   Finished == true
    ->  State = State1
    ;   process_tool_calls(Calls, State1, State)
    ).

call_field(Call, Key, Default, Value) :-
    ( is_dict(Call), get_dict(Key, Call, V) -> dc_json:as_text(V, Value) ; Value = Default ).
call_arguments(Call, Arguments) :-
    ( is_dict(Call), get_dict(arguments, Call, A), is_dict(A) -> Arguments = A ; Arguments = _{} ).

process_finish(Id, Arguments, State0, State) :-
    (   get_dict(success, Arguments, Success0),
        ( Success0 == true ; Success0 == "true" )
    ->  Success = true
    ;   Success = false
    ),
    get_dict(variables, State0, Variables),
    (   Success == true
    ->  required_outputs(State0, Required),
        findall(Missing, (member(Name, Required), \+ get_dict(Name, Variables, _)), Missing),
        (   Missing == []
        ->  append_tool_message(State0, "finish", Id,
                "{\"finished\":true,\"success\":true}", State1),
            State = State1.put(finished, true).put(success, true)
        ;   format(string(Message), "Cannot finish: missing result variable(s): ~w", [Missing]),
            append_tool_message(State0, "finish", Id, Message, State)
        )
    ;   append_tool_message(State0, "finish", Id,
            "{\"finished\":true,\"success\":false}", State1),
        State = State1.put(finished, true).put(success, false)
    ).

process_set_result(Id, Arguments, State0, State) :-
    (   get_dict(variable, Arguments, RawName)
    ->  dc_json:as_atom(RawName, Name0)
    ;   Name0 = ''
    ),
    get_dict(variables, State0, Variables0),
    (   get_dict(value, Arguments, Value0)
    ->  Value0 = Value1
    ;   Value1 = null
    ),
    (   Name0 == ''
    ->  required_outputs(State0, [Only]),
        Name = Only
    ;   Name = Name0
    ),
    (   valid_output_name(State0, Name)
    ->  output_type_for(State0, Name, Type),
        coerce_value(Type, Value1, Value),
        (   valid_value(Type, Value)
        ->  put_dict(Name, Variables0, Value, Variables),
            Result = _{success: true, variable: Name, value: Value},
            dc_json:json_text(Result, ResultText),
            append_tool_message(State0, "set_result", Id, ResultText, State1),
            State = State1.put(variables, Variables)
        ;   format(string(Message), "set_result(~w): expected ~w", [Name, Type]),
            append_tool_message(State0, "set_result", Id, Message, State)
        )
    ;   append_tool_message(State0, "set_result", Id,
            "Unknown result variable", State)
    ).

required_outputs(State, Names) :-
    ( get_dict(output_vars, State, OutputVars) -> true ; OutputVars = [] ),
    findall(Name, (member(V, OutputVars), output_name(V, Name)), Names).

valid_output_name(State, Name) :-
    required_outputs(State, Names),
    memberchk(Name, Names).

output_type_for(State, Name, Type) :-
    ( get_dict(output_vars, State, OutputVars) -> true ; OutputVars = [] ),
    member(V, OutputVars),
    output_name(V, Name),
    output_type(V, Type),
    !.
output_type_for(_, _, "string").

coerce_value(Type, Value0, Value) :-
    (   string(Value0), Type \== "string"
    ->  catch(dc_json:json_parse(Value0, Parsed), _, fail)
    ->  Value = Parsed
    ;   Value = Value0
    ).
valid_value(Type, Value) :-
    (   Type == "string" -> string(Value)
    ;   Type == "integer" -> integer(Value)
    ;   Type == "number" -> number(Value)
    ;   Type == "boolean" -> (Value == true ; Value == false)
    ;   Type == "array" -> is_list(Value)
    ;   Type == "object" -> is_dict(Value)
    ;   true
    ).

process_user_tool(Name, Id, Arguments, State0, State) :-
    get_dict(session, State0, Session),
    get_dict(messages, State0, Messages),
    get_dict(tool_info, State0, ToolInfos),
    (   member(ToolInfo, ToolInfos), tool_info_name(ToolInfo, Name)
    ->  dc_events:emit(tool_call(started, Name, Arguments)),
        catch(
            execute_dml_tool(Session, ToolInfo, Arguments, Result),
            Error,
            (   format(string(Message), "~w", [Error]),
                Result = _{error: Message}
            )
        ),
        dc_events:emit(tool_call(completed, Name, Result)),
        dc_json:result_text(Result, Text),
        ToolMessage = llm_message{role: tool, content: Text, tool_call_id: Id, tool_name: Name},
        append(Messages, [ToolMessage], Messages1),
        State = State0.put(messages, Messages1)
    ;   format(string(Message), "Tool not found: ~w", [Name]),
        ToolMessage = llm_message{role: tool, content: Message, tool_call_id: Id, tool_name: Name},
        append(Messages, [ToolMessage], Messages1),
        State = State0.put(messages, Messages1)
    ).

%% ---------------------------------------------------------------
%% DML tool execution
%% ---------------------------------------------------------------

execute_dml_tool(Session, ToolInfo, Arguments, Result) :-
    tool_info_name(ToolInfo, Name),
    tool_args_positional(ToolInfo, Arguments, Args),
    deepclause_mi:create_tool_engine(Session, Name, Args, EngineId),
    catch(
        run_tool_engine(Session, EngineId, Result),
        Error,
        ( deepclause_mi:destroy_tool_engine(EngineId), throw(Error) )
    ),
    deepclause_mi:destroy_tool_engine(EngineId).

tool_args_positional(ToolInfo, Arguments, Args) :-
    is_dict(ToolInfo),
    get_dict(schema, ToolInfo, Schema),
    (   is_dict(Schema), get_dict(inputs, Schema, Inputs), is_list(Inputs)
    ->  maplist(arg_value(Arguments), Inputs, Args)
    ;   Args = []
    ).

arg_value(Arguments, Param, Value) :-
    (   is_dict(Param), get_dict(name, Param, Name0)
    ->  dc_json:key_atom(Name0, Name)
    ;   Name = arg1
    ),
    (   is_dict(Arguments), get_dict(Name, Arguments, Value0)
    ->  Value = Value0
    ;   Value = null
    ).

run_tool_engine(Session, EngineId, Result) :-
    deepclause_mi:step_tool_engine(EngineId, Status, Content, Payload),
    (   Status == finished
    ->  (   is_dict(Payload), get_dict(result, Payload, R)
        ->  Result = R
        ;   Result = true
        )
    ;   Status == request_exec
    ->  get_dict(toolName, Payload, Tool0),
        dc_json:as_atom(Tool0, Tool),
        get_dict(args, Payload, ToolArgs),
        dc_tools:exec_payload(payload{toolName: Tool, args: ToolArgs}, ExecResult, Success),
        ( Success == true -> deepclause_mi:post_exec_result(Session, success, ExecResult)
        ;   deepclause_mi:post_exec_result(Session, failure, ExecResult)
        ),
        run_tool_engine(Session, EngineId, Result)
    ;   Status == request_llm
    ->  get_dict(messages, Payload, Messages),
        (   catch(dc_llm:complete_messages(Messages, Response), Error,
                  (format(string(_), "~w", [Error]), fail))
        ->  get_dict(text, Response, Text),
            deepclause_mi:post_llm_result(Session, success, Text)
        ;   deepclause_mi:post_llm_result(Session, failure, "LLM request failed")
        ),
        run_tool_engine(Session, EngineId, Result)
    ;   Status == request_agent_loop
    ->  run(Session, Payload, Nested),
        get_dict(success, Nested, NSuccess),
        get_dict(variables, Nested, NVars),
        get_dict(messages, Nested, NMessages),
        get_dict(full_memory, Nested, NFull),
        deepclause_mi:post_tool_agent_result(Session, EngineId, NSuccess, NVars, NMessages, NFull),
        run_tool_engine(Session, EngineId, Result)
    ;   Status == output
    ->  dc_events:emit(output(Content)),
        run_tool_engine(Session, EngineId, Result)
    ;   Status == error
    ->  throw(error(tool_engine(Content)))
    ;   % Unknown request: do not hang; report it as a tool failure.
        format(string(Message), "unsupported tool-engine status: ~w", [Status]),
        throw(error(tool_engine(Message)))
    ).

%% ---------------------------------------------------------------
%% LLM events and nudges
%% ---------------------------------------------------------------

llm_event(text(Text)) :- dc_events:emit(stream(Text)).
llm_event(reasoning(Text)) :- dc_events:emit(reasoning(Text)).
llm_event(usage(Usage)) :- dc_events:emit(usage(Usage)).
llm_event(error(Message)) :- dc_events:emit(error(Message)).
llm_event(done(_)).
llm_event(_).

maybe_nudge(State0, State) :-
    get_dict(output_vars, State0, OutputVars),
    OutputVars \== [],
    get_dict(variables, State0, Variables),
    required_outputs(State0, Required),
    findall(Missing, (member(Name, Required), \+ get_dict(Name, Variables, _)), Missing),
    Missing \== [],
    get_dict(nudges, State0, Nudges),
    Nudges < 2,
    !,
    get_dict(messages, State0, Messages),
    format(string(Message),
        "You still need to set these result variables: ~w. Call set_result for each, then finish(true).",
        [Missing]),
    append(Messages, [llm_message{role: user, content: Message}], Messages1),
    State = State0.put(messages, Messages1).put(nudges, Nudges + 1).
maybe_nudge(State, State).

append_tool_message(State0, Name, Id, Text, State) :-
    get_dict(messages, State0, Messages),
    append(Messages, [llm_message{role: tool, content: Text, tool_call_id: Id, tool_name: Name}], Messages1),
    State = State0.put(messages, Messages1).

keep_memory(Messages, Out) :-
    findall(M,
        (   member(Msg, Messages),
            message_role(Msg, Role),
            ( Role == user ; Role == assistant ),
            message_content(Msg, Content),
            ( Content \== "" -> true ; Role == user ),
            M = message{role: Role, content: Content}
        ),
        Out).

message_content(Message, Text) :-
    ( is_dict(Message), get_dict(content, Message, C) -> dc_json:as_text(C, Text) ; Text = "" ).
