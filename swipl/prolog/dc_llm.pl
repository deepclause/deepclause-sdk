%% Normalized LLM transport for the plain SWI runtime.
%%
%% A provider module implements:
%%   request(+Request, -Http)
%%   event(+EventName, +DataString, +State0, -State, :Callback)
%%   finish(+State, -Response)
%%   decode_json(+JsonTerm, :Callback, -Response)      (non-stream response)
%%
%% Request and Response are documented in docs/SWIPL_RUNTIME.md.
:- module(dc_llm, [
    stream/3,
    complete/2,
    complete_messages/2,
    complete_messages/3,
    request_from_messages/3,
    emit_event/2
]).

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(readutil)).
:- use_module(library(filesex)).
:- use_module(dc_config).
:- use_module(dc_json).
:- use_module(dc_sse).
:- use_module(dc_events).

:- dynamic provider_dir/1.
:- prolog_load_context(directory, Here),
   atomic_list_concat([Here, '/providers'], Dir),
   assertz(provider_dir(Dir)).

%% stream(+Request, :Callback, -Response)
stream(Request0, Callback, Response) :-
    normalize_request(Request0, Request),
    get_dict(provider, Request, Provider),
    dc_config:provider_adapter(Provider, Adapter),
    ensure_provider_loaded(Adapter),
    call(Adapter:request(Request, Http)),
    catch(
        stream_http(Adapter, Request, Http, Callback, Response),
        Error,
        (   format(string(Message), "LLM request failed: ~w", [Error]),
            emit_event(Callback, error(Message)),
            throw(Error)
        )
    ).

%% complete(+Request, -Response)
complete(Request, Response) :-
    stream(Request, none, Response).

%% complete_messages(+Messages, -Response)
complete_messages(Messages, Response) :-
    complete_messages(Messages, none, Response).

%% complete_messages(+Messages, :Callback, -Response)
complete_messages(Messages, Callback, Response) :-
    request_from_messages(Messages, [], Request),
    stream(Request, Callback, Response).

%% request_from_messages(+Messages, +Tools, -Request)
request_from_messages(Messages, Tools, Request) :-
    dc_config:provider(Provider),
    dc_config:model(Model),
    dc_config:temperature(Temperature),
    dc_config:max_tokens(MaxTokens),
    Request = llm_request{
        provider: Provider,
        model: Model,
        messages: Messages,
        tools: Tools,
        temperature: Temperature,
        max_tokens: MaxTokens,
        reasoning: none,
        stream: true
    }.

normalize_request(Request0, Request) :-
    (   get_dict(provider, Request0, Provider)
    ->  true
    ;   dc_config:provider(Provider)
    ),
    (   get_dict(model, Request0, Model)
    ->  true
    ;   dc_config:model(Model)
    ),
    (   get_dict(temperature, Request0, Temperature)
    ->  true
    ;   dc_config:temperature(Temperature)
    ),
    (   get_dict(max_tokens, Request0, MaxTokens)
    ->  true
    ;   dc_config:max_tokens(MaxTokens)
    ),
    (   get_dict(tools, Request0, Tools)
    ->  true
    ;   Tools = []
    ),
    (   get_dict(stream, Request0, Stream)
    ->  true
    ;   Stream = true
    ),
    (   get_dict(reasoning, Request0, Reasoning0)
    ->  Reasoning = Reasoning0
    ;   Reasoning = none
    ),
    Request = llm_request{
        provider: Provider,
        model: Model,
        messages: Request0.messages,
        tools: Tools,
        temperature: Temperature,
        max_tokens: MaxTokens,
        reasoning: Reasoning,
        stream: Stream
    }.

stream_http(Adapter, Request, Http, Callback, Response) :-
    get_dict(url, Http, URL),
    get_dict(headers, Http, Headers0),
    get_dict(body, Http, Body),
    (   get_dict(method, Http, Method) -> true ; Method = post ),
    (   get_dict(timeout, Http, Timeout0) -> true ; dc_config:timeout(Timeout0) ),
    Timeout = Timeout0,
    dc_json:json_text(Body, BodyText),
    maplist(header_option, Headers0, HeaderOptions),
    http_open(URL, In, [
        method(Method),
        post(bytes('application/json', BodyText)),
        status_code(Status),
        header(content_type, ContentType),
        timeout(Timeout)
        | HeaderOptions
    ]),
    setup_call_cleanup(
        true,
        (   (   Status >= 200, Status < 300
            ->  read_success(Adapter, Request, In, ContentType, Callback, Response)
            ;   read_all(In, BodyTextOut),
                throw(error(llm_http(Status, BodyTextOut)))
            )
        ),
        close(In)
    ).

read_success(Adapter, Request, In, ContentType, Callback, Response) :-
    nb_delete(llm_state),
    (   is_sse_content_type(ContentType)
    ->  State0 = llm_state{
            text: "",
            reasoning: "",
            tool_calls: [],
            usage: usage{input: 0, output: 0, total: 0,
                         cache_read: 0, cache_write: 0, reasoning: 0},
            finish_reason: none,
            raw: none
        },
        dc_sse:stream_events(In, dc_llm:handle_sse(Adapter, Request, Callback, State0)),
        % The state is threaded in the process dictionary by the callback; this
        % avoids a mutable state object crossing the SSE callback boundary.
        (   nb_current(llm_state, FinalState)
        ->  true
        ;   FinalState = State0
        ),
        nb_delete(llm_state),
        call(Adapter:finish(FinalState, Response)),
        ( get_dict(usage, Response, Usage) -> emit_event(Callback, usage(Usage)) ; true ),
        emit_event(Callback, done(Response))
    ;   json_read_dict(In, Json, []),
        call(Adapter:decode_json(Json, Callback, Response)),
        ( get_dict(usage, Response, Usage) -> emit_event(Callback, usage(Usage)) ; true ),
        emit_event(Callback, done(Response))
    ).

handle_sse(Adapter, Request, Callback, InitialState, EventName, Data) :-
    (   nb_current(llm_state, State0)
    ->  true
    ;   State0 = InitialState
    ),
    catch(
        call(Adapter:event(EventName, Data, Request, State0, State, Callback)),
        Error,
        (   format(string(Message), "provider stream error: ~w", [Error]),
            emit_event(Callback, error(Message)),
            State = State0
        )
    ),
    nb_setval(llm_state, State).

header_option(Name-Value, request_header(Name=Value)).
header_option(Name=Value, request_header(Name=Value)).

is_sse_content_type(ContentType) :-
    dc_json:as_text(ContentType, Text),
    (   sub_string(Text, _, _, _, "text/event-stream")
    ->  true
    ;   sub_string(Text, _, _, _, "application/x-ndjson")
    ).

read_all(In, Text) :-
    read_string(In, _, Text).

emit_event(none, _) :- !.
emit_event(Callback, Event) :-
    catch(call(Callback, Event), _, true).

ensure_provider_loaded(Module) :-
    (   current_predicate(Module:request/2)
    ->  true
    ;   provider_file(Module, File),
        use_module(File)
    ).

provider_file(Module, File) :-
    (   source_file(dc_llm:_, Source)
    ->  file_directory_name(Source, Base)
    ;   provider_dir(Base)
    ),
    atomic_list_concat([Base, '/providers/', Module, '.pl'], PlFile),
    (   exists_file(PlFile)
    ->  File = PlFile
    ;   atomic_list_concat([Base, '/providers/', Module, '.qlf'], File)
    ).
