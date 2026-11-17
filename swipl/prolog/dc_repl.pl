%% Plain SWI-Prolog DeepClause REPL.
:- module(dc_repl, [main/0, main/1, print_event/1]).

:- use_module(dc_config).
:- use_module(dc_events).
:- use_module(dc_session, [new_session/2, destroy/1, reset/1, load_file/2,
                          run_main/3, list_harness/2, save_harness/2]).
:- use_module(dc_json).

main :- main([]).

main(Args0) :-
    (   Args0 == []
    ->  current_prolog_flag(argv, Args)
    ;   Args = Args0
    ),
    parse_args(Args, Options, Files, Help),
    (   Help == true
    ->  usage, halt
    ;   true
    ),
    catch(
        ( dc_config:init(Options),
          dc_events:set_handler(dc_repl:print_event),
          dc_config:provider(Provider),
          dc_config:model(Model),
          dc_config:workspace(Workspace),
          dc_session:new_session(Session, Params),
          (   Files = [File|_]
          ->  catch(dc_session:load_file(Session, File), _, true)
          ;   true
          ),
          format("DeepClause · SWI · ~w/~w · ~w~n", [Provider, Model, Workspace]),
          format("Type /help for commands.~n~n", []),
          repl_loop(Session, Params)
        ),
        Error,
        ( format(user_error, "fatal: ~w~n", [Error]), halt(1) )
    ).

parse_args(Args, Options, Files, Help) :-
    parse_args(Args, [], [], false, OptionsRev, FilesRev, Help),
    reverse(OptionsRev, Options),
    reverse(FilesRev, Files).

parse_args([], Options, Files, Help, Options, Files, Help).
parse_args(['--help'|Rest], O, F, _, Options, Files, true) :-
    !, parse_args(Rest, O, F, true, Options, Files, true).
parse_args(['-h'|Rest], O, F, _, Options, Files, true) :-
    !, parse_args(Rest, O, F, true, Options, Files, true).
parse_args(['--provider', P|Rest], O, F, H, Options, Files, Help) :-
    !, parse_args(Rest, [provider-P|O], F, H, Options, Files, Help).
parse_args(['--model', M|Rest], O, F, H, Options, Files, Help) :-
    !, parse_args(Rest, [model-M|O], F, H, Options, Files, Help).
parse_args(['--workspace', W|Rest], O, F, H, Options, Files, Help) :-
    !, parse_args(Rest, [workspace-W|O], F, H, Options, Files, Help).
parse_args(['--config', C|Rest], O, F, H, Options, Files, Help) :-
    !, parse_args(Rest, [config-C|O], F, H, Options, Files, Help).
parse_args([Arg|Rest], O, F, H, Options, Files, Help) :-
    (   sub_atom(Arg, 0, _, _, '-')
    ->  format(user_error, "unknown option: ~w~n", [Arg]),
        parse_args(Rest, O, F, H, Options, Files, Help)
    ;   parse_args(Rest, O, [Arg|F], H, Options, Files, Help)
    ).

repl_loop(Session, Params) :-
    format("dml> "), flush_output,
    read_line_to_string(user_input, Line),
    (   Line == end_of_file
    ->  format("~n", []), dc_session:destroy(Session)
    ;   trim_line(Line, Trimmed),
        (   Trimmed == ""
        ->  repl_loop(Session, Params)
        ;   sub_string(Trimmed, 0, 1, _, "/")
        ->  (   command(Trimmed, Session, Params, Continue)
            ->  ( Continue == quit -> dc_session:destroy(Session)
                ; repl_loop(Session, Params) )
            ;   format(user_error, "unknown command~n", []),
                repl_loop(Session, Params)
            )
        ;   sub_string(Trimmed, 0, 5, _, "goal:")
        ->  sub_string(Trimmed, 5, _, 0, Goal0),
            trim_line(Goal0, Goal),
            run_goal(Session, Goal),
            repl_loop(Session, Params)
        ;   (   sub_string(Trimmed, _, 1, 0, ".")
            ->  run_goal(Session, Trimmed)
            ;   format("Natural-language authoring is not implemented in the plain runtime yet.~n", []),
                format("Use /goal <dml> or /run <file> [args].~n", [])
            ),
            repl_loop(Session, Params)
        )
    ).

trim_line(Line0, Line) :-
    (   atom(Line0) -> atom_string(Line0, S0) ; S0 = Line0 ),
    string_codes(S0, Codes0),
    trim_left_codes(Codes0, Codes1),
    reverse(Codes1, Rev0),
    trim_left_codes(Rev0, Rev1),
    reverse(Rev1, Codes),
    string_codes(Line, Codes).

trim_left_codes([C|Cs], Rest) :-
    memberchk(C, [0' , 0'\t, 0'\r, 0'\n]),
    !,
    trim_left_codes(Cs, Rest).
trim_left_codes(Cs, Cs).

command(Line, Session, Params, Continue) :-
    split_string(Line, " \t", " \t", Parts),
    Parts = [Cmd|Args],
    command_cmd(Cmd, Args, Session, Params, Continue).

command_cmd("/quit", _, _, _, quit) :- !.
command_cmd("/exit", _, _, _, quit) :- !.
command_cmd("/help", _, _, _, continue) :-
    !, usage.
command_cmd("/load", [Path|_], Session, _, continue) :-
    !, ( catch(dc_session:load_file(Session, Path), E,
               (format(user_error, "load failed: ~w~n", [E]), fail))
       -> true ; true ).
command_cmd("/goal", Args, Session, _, continue) :-
    !, atomic_list_concat(Args, " ", Goal),
    run_goal(Session, Goal).
command_cmd("/run", [Path|Args], Session, _, continue) :-
    !,
    (   catch(dc_session:load_file(Session, Path), E,
              (format(user_error, "load failed: ~w~n", [E]), fail))
    ->  (   catch(dc_session:run_main(Session, Args, Outcome), E,
                  (format(user_error, "run failed: ~w~n", [E]), fail))
        ->  true
        ;   true
        ),
        (   nonvar(Outcome), Outcome == finished -> format("done.~n", []) ; true )
    ;   true
    ).
command_cmd("/listing", Args, Session, _, continue) :-
    !,
    ( Args == [] -> Filter = all ; atomic_list_concat(Args, " ", Filter) ),
    catch(dc_session:list_harness(Session, Filter), E,
          format(user_error, "listing failed: ~w~n", [E])).
command_cmd("/save", [Path|_], Session, _, continue) :-
    !, catch(dc_session:save_harness(Session, Path), E,
             format(user_error, "save failed: ~w~n", [E])).
command_cmd("/reset", _, Session, _, continue) :-
    !, dc_session:reset(Session),
    format("harness reset~n", []).
command_cmd("/provider", [], _, _, continue) :-
    !, dc_config:provider(P), format("~w~n", [P]).
command_cmd("/provider", [P|_], _, _, continue) :-
    !, atom_string(Atom, P), dc_config:set_provider(Atom),
    dc_config:model(M), format("provider ~w · model ~w~n", [Atom, M]).
command_cmd("/model", [], _, _, continue) :-
    !, dc_config:model(M), format("~w~n", [M]).
command_cmd("/model", [M|_], _, _, continue) :-
    !, dc_config:set_model(M), format("model ~w~n", [M]).
command_cmd("/config", _, _, _, continue) :-
    !, dc_config:config(C), dc_json:json_text(C, T), format("~w~n", [T]).
command_cmd(Cmd, _, _, _, _) :-
    format(user_error, "unknown command: ~w~n", [Cmd]),
    fail.

run_goal(Session, Goal) :-
    (   Goal == ""
    ->  true
    ;   catch(dc_frame:run_goal(Session, Goal, [], Outcome), E,
              (format(user_error, "ERROR: ~w~n", [E]), Outcome = error(E))),
        (   Outcome == finished -> true
        ;   Outcome = error(_) -> true
        ;   Outcome = query_error(Message) ->
            format(user_error, "query error: ~w~n", [Message])
        ;   Outcome = no_frame -> format(user_error, "no frame~n", [])
        ;   true
        )
    ).

print_event(stream(Text)) :-
    !, write(Text), flush_output.
print_event(reasoning(Text)) :-
    !, write(Text), flush_output.
print_event(tool_call(started, Name, Args)) :-
    !, format("~n⚙ ~w ~q~n", [Name, Args]).
print_event(tool_call(completed, Name, Result)) :-
    !,
    (   is_dict(Result), get_dict(success, Result, false)
    ->  format("✖ ~w~n", [Name])
    ;   format("✔ ~w~n", [Name])
    ).
print_event(usage(Usage)) :-
    !,
    (   get_dict(total, Usage, Total), Total > 0
    ->  format("~n[tokens ~w]~n", [Total])
    ;   true
    ).
print_event(model_call(started)) :-
    !, format("~n… ~n", []).
print_event(model_call(completed)) :-
    !.
print_event(task_activity(Payload)) :-
    !,
    (   get_dict(taskDescription, Payload, Desc), Desc \== ""
    ->  format("~n— ~w~n", [Desc])
    ;   true
    ).
print_event(error(Message)) :-
    !, format(user_error, "ERROR: ~w~n", [Message]).
print_event(output(Text)) :-
    !, write(Text), flush_output.
print_event(_).

usage :-
    format("DeepClause plain SWI-Prolog REPL~n~n", []),
    format("Commands:~n", []),
    format("  /help                     this help~n", []),
    format("  /load <file.dml>          load a DML harness~n", []),
    format("  /goal <dml>               run a goal~n", []),
    format("  /run <file> [args...]     load and run agent_main/N~n", []),
    format("  /listing [filter]         inspect the harness~n", []),
    format("  /save <file.dml>          export the harness~n", []),
    format("  /reset                    empty the harness~n", []),
    format("  /provider [name]          show/set the LLM provider~n", []),
    format("  /model [id]               show/set the model~n", []),
    format("  /config                   show effective config JSON~n", []),
    format("  /quit                     exit~n~n", []),
    format("Plain text ending in '.' is treated as a DML goal.~n", []),
    format("Providers: openai, anthropic, google, openrouter, azure,~n", []),
    format("           groq, mistral, deepseek, xai, together, openai_compatible.~n", []).
