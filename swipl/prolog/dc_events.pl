%% Tiny event bus so the runtime does not depend on the REPL.
:- module(dc_events, [set_handler/1, clear_handler/0, emit/1]).

:- dynamic handler/1.

set_handler(Handler) :-
    retractall(handler(_)),
    assertz(handler(Handler)).

clear_handler :-
    retractall(handler(_)).

emit(Event) :-
    (   handler(Handler)
    ->  catch(call(Handler, Event), Error,
              (print_message(warning, Error), fail))
    ;   true
    ).
