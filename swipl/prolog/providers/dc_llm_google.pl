%% Google Gemini generateContent adapter.
:- module(dc_llm_google, [request/2, event/6, finish/2, decode_json/3]).

:- use_module(library(lists)).
:- use_module('../dc_config.pl').
:- use_module('../dc_json.pl').
:- use_module('../dc_llm.pl').

request(Request, Http) :-
    get_dict(provider, Request, Provider),
    get_dict(model, Request, Model),
    get_dict(messages, Request, Messages),
    get_dict(tools, Request, Tools),
    get_dict(temperature, Request, Temperature),
    get_dict(max_tokens, Request, MaxTokens),
    dc_config:base_url(Provider, Base0),
    strip_trailing_slash(Base0, Base),
    atomic_list_concat([Base, '/models/', Model, ':streamGenerateContent?alt=sse'], URL),
    split_system(Messages, System, Conversation),
    google_contents(Conversation, Contents),
    google_tools(Tools, GoogleTools),
    Body0 = _{contents: Contents},
    (   System == "" -> Body1 = Body0 ; put_dict(systemInstruction, Body0, _{parts: [_{text: System}]}, Body1) ),
    (   GoogleTools == [] -> Body2 = Body1 ; put_dict(tools, Body1, GoogleTools, Body2) ),
    generation_config(_{}, Config0),
    (   number(Temperature) -> put_dict(temperature, Config0, Temperature, Config1) ; Config1 = Config0 ),
    (   integer(MaxTokens), MaxTokens > 0 -> put_dict(maxOutputTokens, Config1, MaxTokens, Config) ; Config = Config1 ),
    (   Config == _{} -> Body = Body2 ; put_dict(generationConfig, Body2, Config, Body) ),
    dc_config:api_key(Provider, ApiKey),
    Headers = [('x-goog-api-key'-ApiKey)],
    Http = http_request{url: URL, method: post, headers: Headers,
                        body: Body, timeout: 120}.

generation_config(D, D).

strip_trailing_slash(URL, URL) :-
    ( sub_string(URL, _, 1, 0, "/") -> false ; true ).
strip_trailing_slash(URL, Trimmed) :-
    sub_string(URL, _, 1, 0, "/"),
    !,
    sub_string(URL, 0, _, 1, Trimmed).

split_system([], "", []).
split_system([Message|Messages], System, Conversation) :-
    message_role(Message, Role),
    (   Role == system
    ->  message_content_text(Message, Text),
        split_system(Messages, Rest, Conversation),
        ( Rest == "" -> System = Text ; string_concat(Text, "\n\n", T2), string_concat(T2, Rest, System) )
    ;   split_system(Messages, System, RestConversation),
        Conversation = [Message|RestConversation]
    ).

message_role(Message, Role) :-
    ( is_dict(Message), get_dict(role, Message, Role0) -> dc_json:key_atom(Role0, Role) ; Role = user ).
message_content_text(Message, Text) :-
    ( is_dict(Message), get_dict(content, Message, Content0) -> dc_json:as_text(Content0, Text) ; Text = "" ).
message_tool_calls(Message, Calls) :-
    ( is_dict(Message), get_dict(tool_calls, Message, Calls0), is_list(Calls0) -> Calls = Calls0 ; Calls = [] ).
message_tool_call_id(Message, Id) :-
    ( is_dict(Message), get_dict(tool_call_id, Message, Id0) -> dc_json:as_text(Id0, Id) ; Id = "" ).

google_contents(Messages, Out) :-
    fold_contents(Messages, [], Rev),
    reverse(Rev, Ordered),
    pairs_to_contents(Ordered, Out).

pairs_to_contents([], []).
pairs_to_contents([Role-Parts|Pairs], [_{role: Role, parts: Parts}|Out]) :-
    pairs_to_contents(Pairs, Out).

fold_contents([], Acc, Acc).
fold_contents([Message|Messages], Acc, Out) :-
    message_role(Message, Role0),
    ( Role0 == tool -> Role = user ; Role0 == assistant -> Role = model ; Role = Role0 ),
    google_parts(Message, Parts),
    (   Acc = [Role-Previous|Rest]
    ->  append(Previous, Parts, Merged),
        fold_contents(Messages, [Role-Merged|Rest], Out)
    ;   fold_contents(Messages, [Role-Parts|Acc], Out)
    ).

google_parts(Message, Parts) :-
    message_role(Message, Role),
    (   Role == tool
    ->  message_content_text(Message, Text),
        message_tool_name(Message, Name),
        Parts = [_{functionResponse: _{name: Name, response: _{result: Text}}}]
    ;   message_tool_calls(Message, Calls),
        Calls \== []
    ->  message_content_text(Message, Text),
        ( Text == "" -> TextParts = [] ; TextParts = [_{text: Text}] ),
        google_function_calls(Calls, CallParts),
        append(TextParts, CallParts, Parts)
    ;   message_content_text(Message, Text),
        Parts = [_{text: Text}]
    ).

message_tool_name(Message, Name) :-
    ( is_dict(Message), get_dict(tool_name, Message, Name0) -> dc_json:as_text(Name0, Name) ;
      is_dict(Message), get_dict(name, Message, Name1) -> dc_json:as_text(Name1, Name) ;
      Name = "" ).

google_function_calls([], []).
google_function_calls([Call|Calls], [_{functionCall: _{name: Name, args: Args}}|Out]) :-
    call_field(Call, name, "", Name),
    ( is_dict(Call), get_dict(arguments, Call, A), is_dict(A) -> Args = A ; Args = _{} ),
    google_function_calls(Calls, Out).

call_field(Call, Key, Default, Value) :-
    ( is_dict(Call), get_dict(Key, Call, V) -> dc_json:as_text(V, Value) ; Value = Default ).

google_tools([], []).
google_tools(Tools, [_{functionDeclarations: Declarations}]) :-
    google_declarations(Tools, Declarations).

google_declarations([], []).
google_declarations([Tool|Tools], [Out|Outs]) :-
    tool_field(Tool, name, Name),
    tool_field(Tool, description, Description),
    ( is_dict(Tool), get_dict(parameters, Tool, Params0) -> true ; Params0 = _{} ),
    google_schema(Params0, Params),
    Out = _{name: Name, description: Description, parameters: Params},
    google_declarations(Tools, Outs).

tool_field(Tool, Key, Value) :-
    ( is_dict(Tool), get_dict(Key, Tool, V) -> dc_json:as_text(V, Value) ; Value = "" ).

%% Gemini accepts an OpenAPI subset; strip JSON Schema keywords it rejects.
google_schema(Dict0, Dict) :-
    is_dict(Dict0),
    !,
    dict_pairs(Dict0, _, Pairs0),
    include(google_schema_pair, Pairs0, Pairs1),
    dict_create(Dict1, _, Pairs1),
    (   get_dict(properties, Dict1, Props0), is_dict(Props0)
    ->  dict_pairs(Props0, _, PropPairs0),
        maplist(google_schema_property, PropPairs0, PropPairs),
        dict_create(Props, _, PropPairs),
        put_dict(properties, Dict1, Props, Dict2)
    ;   Dict2 = Dict1
    ),
    (   get_dict(items, Dict2, Items0), is_dict(Items0)
    ->  google_schema(Items0, Items),
        put_dict(items, Dict2, Items, Dict)
    ;   Dict = Dict2
    ).
google_schema(Other, Other).

google_schema_pair(Name-_) :-
    \+ memberchk(Name, [additionalProperties, '$schema', title, default]).
google_schema_property(Name-Value, Name-Out) :-
    google_schema(Value, Out).

%% event(+EventName, +Data, +Request, +State0, -State, :Callback)
event(_, "[DONE]", _Request, State, State, _Callback) :- !.
event(_, Data, _Request, State0, State, Callback) :-
    catch(
        (   dc_json:json_parse(Data, Json),
            google_event(Json, State0, State, Callback)
        ),
        Error,
        (   format(string(Message), "Gemini stream parse failed: ~w", [Error]),
            dc_llm:emit_event(Callback, error(Message)),
            State = State0
        )
    ).

google_event(Json, State0, State, Callback) :-
    (   get_dict(candidates, Json, Candidates), is_list(Candidates), Candidates \== []
    ->  Candidates = [Candidate|_],
        (   get_dict(content, Candidate, Content),
            get_dict(parts, Content, Parts), is_list(Parts)
        ->  google_parts_event(Parts, State0, State1, Callback)
        ;   State1 = State0
        ),
        ( get_dict(finishReason, Candidate, Finish0) -> put_dict(finish_reason, State1, Finish0, State2) ; State2 = State1 )
    ;   State2 = State0
    ),
    (   get_dict(usageMetadata, Json, Usage0), is_dict(Usage0)
    ->  normalize_usage(Usage0, Usage), put_dict(usage, State2, Usage, State3)
    ;   State3 = State2
    ),
    (   get_dict(error, Json, Error)
    ->  format(string(Message), "Gemini error: ~w", [Error]), dc_llm:emit_event(Callback, error(Message))
    ;   true
    ),
    State = State3.

google_parts_event([], State, State, _).
google_parts_event([Part|Parts], State0, State, Callback) :-
    (   get_dict(text, Part, Text0)
    ->  dc_json:as_text(Text0, Text),
        (   ( get_dict(thought, Part, true) ; get_dict(thought, Part, "true") )
        ->  append_state_text(State0, reasoning, Text, State1),
            dc_llm:emit_event(Callback, reasoning(Text))
        ;   append_state_text(State0, text, Text, State1),
            dc_llm:emit_event(Callback, text(Text))
        )
    ;   get_dict(functionCall, Part, FunctionCall), is_dict(FunctionCall)
    ->  call_field(FunctionCall, name, "", Name),
        ( get_dict(args, FunctionCall, Args), is_dict(Args) -> true ; Args = _{} ),
        get_dict(tool_calls, State0, Calls0),
        length(Calls0, N0), N is N0 + 1,
        format(string(Id), "call_~w", [N]),
        append(Calls0, [tool_call{id: Id, name: Name, arguments: Args}], Calls),
        put_dict(tool_calls, State0, Calls, State1)
    ;   State1 = State0
    ),
    google_parts_event(Parts, State1, State, Callback).

append_state_text(State0, Key, Text, State) :-
    get_dict(Key, State0, Old0),
    dc_json:as_text(Old0, Old),
    string_concat(Old, Text, New),
    put_dict(Key, State0, New, State).

normalize_usage(Usage0, usage{input: Input, output: Output, total: Total,
                              cache_read: CacheRead, cache_write: 0,
                              reasoning: Reasoning}) :-
    ( get_dict(promptTokenCount, Usage0, I0) -> integer_or(I0, 0, Input) ; Input = 0 ),
    ( get_dict(candidatesTokenCount, Usage0, O0) -> integer_or(O0, 0, Output) ; Output = 0 ),
    ( get_dict(totalTokenCount, Usage0, T0) -> integer_or(T0, Input + Output, Total) ; Total is Input + Output ),
    ( get_dict(cachedContentTokenCount, Usage0, C0) -> integer_or(C0, 0, CacheRead) ; CacheRead = 0 ),
    ( get_dict(thoughtsTokenCount, Usage0, R0) -> integer_or(R0, 0, Reasoning) ; Reasoning = 0 ).

integer_or(Value, _, Value) :- integer(Value), !.
integer_or(_, Default, Default).

finish(State, Response) :-
    get_dict(text, State, Text),
    get_dict(reasoning, State, Reasoning),
    get_dict(tool_calls, State, Calls),
    get_dict(usage, State, Usage),
    get_dict(finish_reason, State, Finish0),
    finish_reason(Finish0, Calls, Finish),
    Response = llm_response{text: Text, reasoning: Reasoning, tool_calls: Calls,
                            usage: Usage, finish_reason: Finish, raw: none}.

finish_reason(_, Calls, tool_use) :- Calls \== [], !.
finish_reason(none, _, stop) :- !.
finish_reason(Reason0, _, Reason) :- dc_json:key_atom(Reason0, Reason).

decode_json(Json, Callback, Response) :-
    (   get_dict(candidates, Json, [Candidate|_]),
        get_dict(content, Candidate, Content),
        get_dict(parts, Content, Parts), is_list(Parts)
    ->  decode_parts(Parts, "", "", [], Text, Reasoning, Calls),
        ( get_dict(usageMetadata, Json, Usage0), is_dict(Usage0) -> normalize_usage(Usage0, Usage)
        ; Usage = usage{input:0, output:0, total:0, cache_read:0, cache_write:0, reasoning:0} ),
        ( get_dict(finishReason, Candidate, Finish0) -> finish_reason(Finish0, Calls, Finish) ; Finish = stop ),
        Response = llm_response{text: Text, reasoning: Reasoning, tool_calls: Calls,
                                usage: Usage, finish_reason: Finish, raw: none},
        ( Text \== "" -> dc_llm:emit_event(Callback, text(Text)) ; true ),
        ( Reasoning \== "" -> dc_llm:emit_event(Callback, reasoning(Reasoning)) ; true )
    ;   Response = llm_response{text:"", reasoning:"", tool_calls:[],
                                usage: usage{input:0, output:0, total:0, cache_read:0, cache_write:0, reasoning:0},
                                finish_reason: stop, raw: none}
    ).

decode_parts([], Text, Reasoning, Calls, Text, Reasoning, Calls).
decode_parts([Part|Parts], Text0, Reasoning0, Calls0, Text, Reasoning, Calls) :-
    (   get_dict(text, Part, T0)
    ->  dc_json:as_text(T0, T),
        ( ( get_dict(thought, Part, true) ; get_dict(thought, Part, "true") )
        ->  string_concat(Reasoning0, T, Reasoning1), Text1 = Text0
        ;   string_concat(Text0, T, Text1), Reasoning1 = Reasoning0
        ),
        decode_parts(Parts, Text1, Reasoning1, Calls0, Text, Reasoning, Calls)
    ;   get_dict(functionCall, Part, FunctionCall), is_dict(FunctionCall)
    ->  call_field(FunctionCall, name, "", Name),
        ( get_dict(args, FunctionCall, Args), is_dict(Args) -> true ; Args = _{} ),
        length(Calls0, N0), N is N0 + 1, format(string(Id), "call_~w", [N]),
        append(Calls0, [tool_call{id: Id, name: Name, arguments: Args}], Calls1),
        decode_parts(Parts, Text0, Reasoning0, Calls1, Text, Reasoning, Calls)
    ;   decode_parts(Parts, Text0, Reasoning0, Calls0, Text, Reasoning, Calls)
    ).
