%% Server-Sent Events reader for provider streaming responses.
:- module(dc_sse, [stream_events/2]).

:- use_module(library(readutil)).

%% stream_events(+Stream, :Callback)
%% Callback is called as call(Callback, EventName, DataString). It returns
%% `stop` to end reading, or any other value to continue. A blank line
%% dispatches the accumulated event; OpenAI's `[DONE]` is passed through and
%% also ends the stream.
stream_events(Stream, Callback) :-
    catch(set_stream(Stream, encoding(utf8)), _, true),
    read_sse(Stream, sse{event: "message", data: []}, Callback).

read_sse(Stream, State0, Callback) :-
    read_line_to_string(Stream, Line0),
    (   Line0 == end_of_file
    ->  flush_event(State0, Callback, _)
    ;   normalize_line(Line0, Line),
        (   Line == ""
        ->  flush_event(State0, Callback, Action),
            (   Action == stop
            ->  true
            ;   read_sse(Stream, sse{event: "message", data: []}, Callback)
            )
        ;   sse_field(Line, State0, State),
            read_sse(Stream, State, Callback)
        )
    ).

normalize_line(Line0, Line) :-
    (   atom(Line0)
    ->  atom_string(Line0, S0)
    ;   S0 = Line0
    ),
    (   sub_string(S0, _, 1, 0, "\r")
    ->  sub_string(S0, 0, _, 1, Line)
    ;   Line = S0
    ).

sse_field(Line, State0, State) :-
    (   sub_string(Line, 0, 1, _, ":")
    ->  State = State0                         % comment / keep-alive
    ;   split_field(Line, Field, Value0),
        strip_one_space(Value0, Value),
        (   Field == "event"
        ->  put_dict(event, State0, Value, State)
        ;   Field == "data"
        ->  get_dict(data, State0, Data0),
            put_dict(data, State0, [Value|Data0], State)
        ;   State = State0                      % id/retry/unknown
        )
    ).

split_field(Line, Field, Value) :-
    (   sub_string(Line, Before, 1, After, ":")
    ->  sub_string(Line, 0, Before, _, Field0),
        Start is Before + 1,
        sub_string(Line, Start, After, 0, Value0),
        Field = Field0, Value = Value0
    ;   Field = Line, Value = ""
    ).

strip_one_space(Value0, Value) :-
    (   sub_string(Value0, 0, 1, _, " ")
    ->  sub_string(Value0, 1, _, 0, Value)
    ;   Value = Value0
    ).

flush_event(State, Callback, Action) :-
    get_dict(data, State, Data0),
    reverse(Data0, Data),
    atomic_list_concat(Data, "\n", Payload),
    (   ( Payload == "" ; Payload == '' )
    ->  Action = continue
    ;   get_dict(event, State, Event),
        (   catch(call(Callback, Event, Payload), Error,
                  (print_message(error, Error), fail))
        ->  (   ( Payload == "[DONE]" ; Payload == '[DONE]' )
            ->  Action = stop
            ;   Action = continue
            )
        ;   Action = continue
        )
    ).
