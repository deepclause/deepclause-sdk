%% Session lifecycle and file operations for the plain REPL.
:- module(dc_session, [new_session/1, new_session/2, destroy/1, reset/1,
                       load_file/2, run_main/3, run_goal/2, list_harness/2,
                       save_harness/2, session_params/1]).

:- use_module(library(filesex)).
:- use_module('../../src/prolog-src/deepclause_mi.pl').
:- use_module(dc_config).
:- use_module(dc_frame).
:- use_module(dc_json).

new_session(Session) :-
    new_session(Session, _).

new_session(Session, Params) :-
    gensym(repl_, Session),
    session_params(Params),
    deepclause_mi:create_session(Session, Params, [], []).

session_params(Params) :-
    dc_config:workspace(Workspace),
    (   getenv('DC_SANDBOX', Sandbox0), Sandbox0 \== ""
    ->  atom_string(Sandbox, Sandbox0)
    ;   Sandbox = clean_room
    ),
    Params = _{workspace_path: Workspace, trace: false, sandbox: Sandbox}.

destroy(Session) :-
    catch(deepclause_mi:destroy_session(Session), _, true).

reset(Session) :-
    catch(deepclause_mi:clear_session(Session), _, true).

load_file(Session, Path) :-
    absolute_file_name(Path, Absolute, [access(read)]),
    read_file_to_string(Absolute, Code, [encoding(utf8)]),
    deepclause_mi:load_code(Session, Code, Error),
    (   Error == none
    ->  format("loaded ~w~n", [Absolute])
    ;   format(user_error, "load error: ~w~n", [Error]),
        fail
    ).

run_main(Session, Args, Outcome) :-
    session_params(Params),
    catch(
        (   deepclause_mi:create_engine(Session, memory, Args, Params, [], _Engine),
            deepclause_mi:default_frame(Session, Frame),
            catch(dc_frame:run_frame(Session, Frame, Outcome0), E,
                  (format(user_error, "ERROR: ~w~n", [E]), Outcome0 = error(E))),
            deepclause_mi:end_frame(Frame),
            Outcome = Outcome0
        ),
        Error,
        (   format(user_error, "run error: ~w~n", [Error]),
            Outcome = error(Error)
        )
    ;
        Outcome = error(run_failed)
    ).

run_goal(Session, Goal) :-
    dc_frame:run_goal(Session, Goal, [frame_kind(object)], _).

quote_args([], []).
quote_args([Arg|Args], [Quoted|QuotedArgs]) :-
    (   string(Arg) -> Text = Arg ; atom_string(Arg, Text) ),
    term_string(Term, Text, [quoted(true)]),
    with_output_to(string(Quoted), write_term(Term, [quoted(true)])),
    quote_args(Args, QuotedArgs).

list_harness(Session, Filter) :-
    deepclause_mi:repl_listing(Session, Filter, Text),
    write(Text).

save_harness(Session, Path) :-
    absolute_file_name(Path, Absolute, []),
    deepclause_mi:export_harness(Session, none, Text),
    setup_call_cleanup(
        open(Absolute, write, Out, [encoding(utf8)]),
        write(Out, Text),
        close(Out)
    ),
    format("saved ~w~n", [Absolute]).
