%% LLM-host probe: prove that the frame host can answer request_llm.
%%
%%   swipl swipl/experiments/llm_probe.pl
%%
%% No network or API key is used. The "provider" is a hard-coded reply; a real
%% host replaces post_llm_result/3 with dc_llm:stream/3.
:- initialization(main, main).

:- prolog_load_context(directory, Here),
   atomic_list_concat([Here, '/../../src/prolog-src'], SrcDir),
   use_module(SrcDir/deepclause_strings),
   use_module(SrcDir/deepclause_memory),
   use_module(SrcDir/deepclause_mi),
   use_module(SrcDir/deepclause_analysis).

main :-
    create_session(probe, _{}, "[]", "[]"),
    load_code(probe, "demo(R) :- llm([user(\"hi\")], R).", LoadError),
    (   LoadError == none
    ->  repl_initial_state(probe, object, State),
        query_start(probe, "demo(R)", object, State, QueryError),
        (   QueryError == none
        ->  default_frame(probe, Frame),
            run_frame(Frame),
            end_frame(Frame)
        ;   format("query error: ~w~n", [QueryError])
        )
    ;   format("load error: ~w~n", [LoadError])
    ),
    destroy_session(probe).

run_frame(Frame) :-
    step_frame(Frame, Status, Content, Payload),
    format("~w ~q ~q~n", [Status, Content, Payload]),
    (   Status == finished
    ->  true
    ;   Status == request_llm
    ->  % A real host calls the provider here and posts the text/reasoning/usage.
        post_llm_result(probe, success, "hello from the fake provider"),
        run_frame(Frame)
    ;   run_frame(Frame)
    ).
