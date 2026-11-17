%% Frame driver: the Prolog equivalent of the TypeScript host loop.
:- module(dc_frame, [run_goal/4, run_frame/3, llm_event/1]).

:- use_module('../../src/prolog-src/deepclause_mi.pl').
:- use_module(dc_agent).
:- use_module(dc_llm).
:- use_module(dc_tools).
:- use_module(dc_events).
:- use_module(dc_json).

%% run_goal(+Session, +GoalText, +Options, -Outcome)
%% Options can include frame_kind(object|query), and silent(true).
run_goal(Session, GoalText, Options, Outcome) :-
    (   memberchk(frame_kind(Kind), Options) -> true ; Kind = object ),
    deepclause_mi:repl_initial_state(Session, Kind, State),
    deepclause_mi:query_start(Session, GoalText, Kind, State, Error),
    (   Error == none
    ->  (   deepclause_mi:default_frame(Session, Frame)
        ->  catch(run_frame(Session, Frame, Outcome0), E,
                  (format(user_error, "ERROR: ~w~n", [E]), Outcome0 = error(E))),
            deepclause_mi:end_frame(Frame),
            Outcome = Outcome0
        ;   Outcome = no_frame
        )
    ;   Outcome = query_error(Error)
    ).

%% run_frame(+Session, +Frame, -Outcome)
run_frame(Session, Frame, Outcome) :-
    deepclause_mi:step_frame(Frame, Status, Content, Payload),
    (   Status == finished
    ->  Outcome = finished
    ;   Status == output
    ->  ( nonvar(Content) -> format("~w~n", [Content]) ; true ),
        run_frame(Session, Frame, Outcome)
    ;   Status == log
    ->  format("~w~n", [Content]),
        run_frame(Session, Frame, Outcome)
    ;   Status == answer
    ->  ( nonvar(Content) -> format("~w~n", [Content]) ; true ),
        run_frame(Session, Frame, Outcome)
    ;   Status == solution
    ->  print_solution(Payload),
        run_frame(Session, Frame, Outcome)
    ;   Status == task_activity
    ->  dc_events:emit(task_activity(Payload)),
        run_frame(Session, Frame, Outcome)
    ;   Status == request_agent_loop
    ->  catch(
            ( dc_agent:run(Session, Payload, Result),
              get_dict(success, Result, Success),
              get_dict(variables, Result, Variables),
              get_dict(messages, Result, Messages),
              get_dict(full_memory, Result, FullMemory),
              get_dict(error, Result, Error),
              deepclause_mi:post_agent_result(Session, Success, Variables, Messages, FullMemory, Error)
            ),
            AgentError,
            ( format(string(Message), "agent error: ~w", [AgentError]),
              dc_events:emit(error(Message)),
              deepclause_mi:post_agent_result(Session, false, _{}, [], [], Message)
            )
        ),
        run_frame(Session, Frame, Outcome)
    ;   Status == request_llm
    ->  catch(
            ( get_dict(messages, Payload, Messages),
              dc_llm:complete_messages(Messages, Response),
              get_dict(text, Response, Text),
              deepclause_mi:post_llm_result(Session, success, Text)
            ),
            LlmError,
            ( format(string(Message), "llm error: ~w", [LlmError]),
              dc_events:emit(error(Message)),
              deepclause_mi:post_llm_result(Session, failure, Message)
            )
        ),
        run_frame(Session, Frame, Outcome)
    ;   Status == request_exec
    ->  catch(
            ( get_dict(toolName, Payload, Tool0),
              dc_json:as_atom(Tool0, Tool),
              get_dict(args, Payload, Args),
              dc_tools:exec_payload(payload{toolName: Tool, args: Args}, Result, Success),
              ( Success == true
              ->  deepclause_mi:post_exec_result(Session, success, Result)
              ;   deepclause_mi:post_exec_result(Session, failure, Result)
              )
            ),
            ExecError,
            ( format(string(Message), "exec error: ~w", [ExecError]),
              deepclause_mi:post_exec_result(Session, failure, _{error: Message})
            )
        ),
        run_frame(Session, Frame, Outcome)
    ;   Status == wait_input
    ->  prompt_input(Content, Answer),
        deepclause_mi:provide_input(Session, Answer),
        run_frame(Session, Frame, Outcome)
    ;   Status == input_required
    ->  prompt_input(Content, Answer),
        deepclause_mi:provide_input(Session, Answer),
        run_frame(Session, Frame, Outcome)
    ;   Status == request_form
    ->  % The plain REPL has no structured form renderer yet; report it rather
        % than silently hanging the engine.
        deepclause_mi:post_form_result(Session, error(unsupported_forms)),
        run_frame(Session, Frame, Outcome)
    ;   Status == wait_form
    ->  deepclause_mi:post_form_result(Session, error(unsupported_forms)),
        run_frame(Session, Frame, Outcome)
    ;   Status == wait_next
    ->  prompt_input("input", Answer),
        deepclause_mi:post_next_result(Session, user(Answer)),
        run_frame(Session, Frame, Outcome)
    ;   Status == request_proc
    ->  (   is_dict(Payload), get_dict(op, Payload, Op)
        ->  (   Op == self
            ->  deepclause_mi:post_proc_result(Session, self, self)
            ;   deepclause_mi:post_proc_result(Session, Op, error(unsupported))
            ),
            dc_events:emit(error("process primitives are not implemented in the plain runtime"))
        ;   true
        ),
        run_frame(Session, Frame, Outcome)
    ;   Status == media
    ->  ( get_dict(path, Payload, Path) -> format("[media ~w]~n", [Path]) ; true ),
        run_frame(Session, Frame, Outcome)
    ;   Status == html
    ->  ( get_dict(path, Payload, Path) -> format("[html ~w]~n", [Path]) ; true ),
        run_frame(Session, Frame, Outcome)
    ;   Status == suspend
    ->  ( get_dict(continuation, Payload, C) -> format("[suspended: ~w]~n", [C]) ; true ),
        Outcome = suspended
    ;   Status == error
    ->  format(user_error, "ERROR: ~w~n", [Content]),
        Outcome = error(Content)
    ;   Status == meta_result
    ->  ( get_dict(answer, Payload, Answer), Answer \== "" -> format("~w~n", [Answer]) ; true ),
        run_frame(Session, Frame, Outcome)
    ;   format(user_error, "ERROR: unknown engine status ~w~n", [Status]),
        Outcome = error(Status)
    ).

print_solution(Payload) :-
    (   get_dict(bindings, Payload, Bindings), is_list(Bindings)
    ->  ( Bindings == [] -> format("true.~n", [])
        ; forall(member(Name-Value, Bindings), print_binding(Name, Value))
        )
    ;   true
    ).

print_binding(Name, Value) :-
    (   var(Value)
    ->  true
    ;   Value == '_'
    ->  true
    ;   parse_dict_text(Value, Dict)
    ->  (   catch(dc_json:json_text(Dict, Json), _, fail)
        ->  format("~w = ~w~n", [Name, Json])
        ;   format("~w = ~w~n", [Name, Value])
        )
    ;   format("~w = ~w~n", [Name, Value])
    ).

parse_dict_text(Text, Dict) :-
    (   string(Text)
    ->  String = Text
    ;   atom(Text)
    ->  atom_string(Text, String)
    ;   fail
    ),
    catch(term_string(Term, String, []), _, fail),
    is_dict(Term),
    Dict = Term.

prompt_input(Prompt, Answer) :-
    format("~w~n> ", [Prompt]),
    flush_output,
    (   read_line_to_string(user_input, Line)
    ->  ( Line == end_of_file -> Answer = "" ; Answer = Line )
    ;   Answer = ""
    ).

llm_event(text(Text)) :- dc_events:emit(stream(Text)).
llm_event(reasoning(Text)) :- dc_events:emit(reasoning(Text)).
llm_event(usage(Usage)) :- dc_events:emit(usage(Usage)).
llm_event(error(Message)) :- dc_events:emit(error(Message)).
llm_event(done(_)).
llm_event(_).
