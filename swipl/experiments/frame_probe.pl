%% Frame-host probe: drive deepclause_mi without a TypeScript host.
%%
%%   swipl swipl/experiments/frame_probe.pl
%%
%% This is the smallest proof that a plain SWI runtime is possible: create a
%% session, start a goal, step the frame, and print the solution/finished events.
:- initialization(main, main).

:- prolog_load_context(directory, Here),
   atomic_list_concat([Here, '/../../src/prolog-src'], SrcDir),
   use_module(SrcDir/deepclause_strings),
   use_module(SrcDir/deepclause_memory),
   use_module(SrcDir/deepclause_mi),
   use_module(SrcDir/deepclause_analysis).

main :-
    create_session(probe, _{}, "[]", "[]"),
    repl_initial_state(probe, object, State),
    query_start(probe, "member(X,[1,2,3])", object, State, Error),
    (   Error == none
    ->  default_frame(probe, Frame),
        run_frame(Frame),
        end_frame(Frame)
    ;   format("query error: ~w~n", [Error])
    ),
    destroy_session(probe).

run_frame(Frame) :-
    step_frame(Frame, Status, Content, Payload),
    format("~w ~q ~q~n", [Status, Content, Payload]),
    (   Status == finished
    ->  true
    ;   run_frame(Frame)
    ).
