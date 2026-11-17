%% OpenAI Chat Completions adapter.
%%
%% Covers OpenAI itself and every OpenAI-compatible endpoint: OpenRouter,
%% Groq, Mistral, DeepSeek, xAI, Together, Fireworks, Cerebras, NVIDIA, and
%% local Ollama/LM Studio/vLLM servers.
:- module(dc_llm_openai, [request/2, event/6, finish/2, decode_json/3]).

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
    openai_url(Provider, Base, URL),
    openai_messages(Messages, OpenAIMessages),
    openai_tools(Tools, OpenAITools),
    Body0 = _{model: Model, messages: OpenAIMessages, stream: true,
              stream_options: _{include_usage: true}},
    (   number(Temperature) -> put_dict(temperature, Body0, Temperature, Body1) ; Body1 = Body0 ),
    (   integer(MaxTokens), MaxTokens > 0 -> put_dict(max_tokens, Body1, MaxTokens, Body2) ; Body2 = Body1 ),
    (   OpenAITools == [] -> Body = Body2 ; put_dict(tools, Body2, OpenAITools, Body) ),
    (   get_dict(reasoning, Request, Reasoning), Reasoning \== none
    ->  put_dict(reasoning_effort, Body, Reasoning, BodyWithReasoning)
    ;   BodyWithReasoning = Body
    ),
    openai_headers(Provider, Headers),
    Http = http_request{url: URL, method: post, headers: Headers,
                        body: BodyWithReasoning, timeout: 120}.

strip_trailing_slash(URL, URL) :-
    (   sub_string(URL, _, 1, 0, "/")
    ->  false
    ;   true
    ).
strip_trailing_slash(URL, Trimmed) :-
    sub_string(URL, _, 1, 0, "/"),
    !,
    sub_string(URL, 0, _, 1, Trimmed).

openai_messages([], []).
openai_messages([Message|Messages], [Out|Outs]) :-
    message_role(Message, Role),
    message_content(Message, Content0),
    dc_json:as_text(Content0, Content),
    (   Role == tool
    ->  message_tool_call_id(Message, CallId),
        Out = _{role: tool, tool_call_id: CallId, content: Content}
    ;   Role == assistant,
        message_tool_calls(Message, Calls),
        Calls \== []
    ->  openai_tool_calls(Calls, OpenAICalls),
        Out = _{role: assistant, content: Content, tool_calls: OpenAICalls}
    ;   Out = _{role: Role, content: Content}
    ),
    openai_messages(Messages, Outs).

message_role(Message, Role) :-
    (   is_dict(Message), get_dict(role, Message, Role0)
    ->  dc_json:key_atom(Role0, Role)
    ;   Role = user
    ).
message_content(Message, Content) :-
    (   is_dict(Message), get_dict(content, Message, Content0)
    ->  Content = Content0
    ;   Content = ""
    ).
message_tool_calls(Message, Calls) :-
    (   is_dict(Message), get_dict(tool_calls, Message, Calls0), is_list(Calls0)
    ->  Calls = Calls0
    ;   Calls = []
    ).
message_tool_call_id(Message, Id) :-
    (   is_dict(Message), get_dict(tool_call_id, Message, Id0)
    ->  dc_json:as_text(Id0, Id)
    ;   Id = ""
    ).

openai_tool_calls([], []).
openai_tool_calls([Call|Calls], [Out|Outs]) :-
    call_id(Call, Id),
    call_name(Call, Name),
    call_arguments(Call, Arguments),
    dc_json:json_text(Arguments, ArgumentsText),
    Out = _{id: Id, type: function,
            function: _{name: Name, arguments: ArgumentsText}},
    openai_tool_calls(Calls, Outs).

call_id(Call, Id) :-
    (   is_dict(Call), get_dict(id, Call, Id0)
    ->  dc_json:as_text(Id0, Id)
    ;   Id = ""
    ).
call_name(Call, Name) :-
    (   is_dict(Call), get_dict(name, Call, Name0)
    ->  dc_json:as_text(Name0, Name)
    ;   Name = ""
    ).
call_arguments(Call, Arguments) :-
    (   is_dict(Call), get_dict(arguments, Call, Arguments0)
    ->  ( is_dict(Arguments0) -> Arguments = Arguments0 ; Arguments = _{} )
    ;   Arguments = _{}
    ).

openai_tools([], []).
openai_tools([Tool|Tools], [Out|Outs]) :-
    tool_name(Tool, Name),
    describe_tool(Tool, Description),
    tool_parameters(Tool, Parameters),
    Out = _{type: function,
            function: _{name: Name, description: Description,
                        parameters: Parameters}},
    openai_tools(Tools, Outs).

tool_name(Tool, Name) :-
    (   is_dict(Tool), get_dict(name, Tool, Name0) -> dc_json:as_text(Name0, Name) ; Name = "" ).
describe_tool(Tool, Description) :-
    (   is_dict(Tool), get_dict(description, Tool, Description0) -> dc_json:as_text(Description0, Description) ; Description = "" ).
tool_parameters(Tool, Parameters) :-
    (   is_dict(Tool), get_dict(parameters, Tool, Parameters0) -> Parameters = Parameters0 ; Parameters = _{} ).

%% event(+EventName, +Data, +Request, +State0, -State, :Callback)
event(_, Done, _Request, State, State, _Callback) :-
    ( Done == "[DONE]" ; Done == '[DONE]' ),
    !.
event(_, Data, _Request, State0, State, Callback) :-
    catch(
        (   dc_json:json_parse(Data, Json),
            openai_event(Json, State0, State, Callback)
        ),
        Error,
        (   format(string(Message), "OpenAI stream parse failed: ~w", [Error]),
            dc_llm:emit_event(Callback, error(Message)),
            State = State0
        )
    ).

openai_event(Json, State0, State, Callback) :-
    (   get_dict(choices, Json, Choices), is_list(Choices), Choices \== []
    ->  Choices = [Choice|_],
        (   get_dict(delta, Choice, Delta)
        ->  openai_delta(Delta, State0, State1, Callback)
        ;   State1 = State0
        ),
        (   get_dict(finish_reason, Choice, Finish0), Finish0 \== null
        ->  put_dict(finish_reason, State1, Finish0, State2)
        ;   State2 = State1
        )
    ;   State2 = State0
    ),
    (   get_dict(usage, Json, Usage0), is_dict(Usage0)
    ->  normalize_usage(Usage0, Usage),
        put_dict(usage, State2, Usage, State3)
    ;   State3 = State2
    ),
    (   get_dict(error, Json, Error)
    ->  format(string(Message), "provider error: ~w", [Error]),
        dc_llm:emit_event(Callback, error(Message)),
        State = State3
    ;   State = State3
    ).

openai_delta(Delta, State0, State, Callback) :-
    (   get_dict(content, Delta, Content), Content \== null
    ->  dc_json:as_text(Content, Text),
        append_state_text(State0, text, Text, State1),
        dc_llm:emit_event(Callback, text(Text))
    ;   State1 = State0
    ),
    (   ( get_dict(reasoning_content, Delta, Reasoning)
        ; get_dict(reasoning, Delta, Reasoning) ),
        Reasoning \== null
    ->  dc_json:as_text(Reasoning, RText),
        append_state_text(State1, reasoning, RText, State2),
        dc_llm:emit_event(Callback, reasoning(RText))
    ;   State2 = State1
    ),
    (   get_dict(tool_calls, Delta, ToolCalls), is_list(ToolCalls)
    ->  update_tool_calls(State2, ToolCalls, State)
    ;   State = State2
    ).

update_tool_calls(State0, [], State0) :- !.
update_tool_calls(State0, [Call|Calls], State) :-
    get_dict(tool_calls, State0, Existing),
    (   get_dict(index, Call, Index) -> true ; Index = 0 ),
    (   get_dict(id, Call, Id0) -> dc_json:as_text(Id0, Id) ; Id = "" ),
    (   get_dict(function, Call, Function) -> true ; Function = _{} ),
    (   get_dict(name, Function, Name0) -> dc_json:as_text(Name0, Name) ; Name = "" ),
    (   get_dict(arguments, Function, Args0) -> dc_json:as_text(Args0, Args) ; Args = "" ),
    update_tool_call(Existing, Index, Id, Name, Args, Updated),
    put_dict(tool_calls, State0, Updated, State1),
    update_tool_calls(State1, Calls, State).

update_tool_call([], Index, Id, Name, Args, [tc{index: Index, id: Id, name: Name, args: Args}]).
update_tool_call([tc{index: I, id: Id0, name: Name0, args: Args0}|Rest], Index, Id, Name, Args,
                 [tc{index: Index, id: Id1, name: Name1, args: Args1}|Rest]) :-
    I == Index,
    !,
    ( Id == "" -> Id1 = Id0 ; Id1 = Id ),
    ( Name == "" -> Name1 = Name0 ; Name1 = Name ),
    string_concat(Args0, Args, Args1).
update_tool_call([Call|Rest], Index, Id, Name, Args, [Call|Updated]) :-
    update_tool_call(Rest, Index, Id, Name, Args, Updated).

append_state_text(State0, Key, Text, State) :-
    get_dict(Key, State0, Old0),
    dc_json:as_text(Old0, Old),
    string_concat(Old, Text, New),
    put_dict(Key, State0, New, State).

%% finish(+State, -Response)
finish(State, Response) :-
    get_dict(text, State, Text),
    get_dict(reasoning, State, Reasoning),
    get_dict(tool_calls, State, Calls0),
    sort_tool_calls(Calls0, Calls1),
    finalize_tool_calls(Calls1, Calls),
    get_dict(usage, State, Usage),
    get_dict(finish_reason, State, Finish0),
    finish_reason(Finish0, Calls, Finish),
    Response = llm_response{text: Text, reasoning: Reasoning, tool_calls: Calls,
                            usage: Usage, finish_reason: Finish, raw: none}.

sort_tool_calls(Calls0, Calls) :-
    predsort(compare_tool_call, Calls0, Calls).

compare_tool_call(Order, tc{index: A}, tc{index: B}) :-
    compare(Order, A, B).

finalize_tool_calls([], []).
finalize_tool_calls([Call|Calls],
                    [tool_call{id: Id, name: Name, arguments: Arguments}|Out]) :-
    get_dict(id, Call, Id),
    get_dict(name, Call, Name),
    get_dict(args, Call, ArgsText),
    parse_arguments(ArgsText, Arguments),
    finalize_tool_calls(Calls, Out).

parse_arguments(Text, Arguments) :-
    (   Text == ""
    ->  Arguments = _{}
    ;   catch(dc_json:json_parse(Text, Parsed),
              _, fail),
        (   is_dict(Parsed)
        ->  Arguments = Parsed
        ;   Arguments = _{value: Parsed}
        )
    ->  true
    ;   Arguments = _{}
    ).

finish_reason(_, Calls, tool_calls) :- Calls \== [], !.
finish_reason(none, _, stop) :- !.
finish_reason(Reason0, _, Reason) :-
    dc_json:key_atom(Reason0, Reason).

normalize_usage(Usage0, usage{input: Input, output: Output, total: Total,
                              cache_read: CacheRead, cache_write: CacheWrite,
                              reasoning: Reasoning}) :-
    get_dict(prompt_tokens, Usage0, Input0),
    get_dict(completion_tokens, Usage0, Output0),
    get_dict(total_tokens, Usage0, Total0),
    (   get_dict(prompt_tokens_details, Usage0, PromptDetails),
        get_dict(cached_tokens, PromptDetails, CacheRead0)
    ->  true
    ;   CacheRead0 = 0
    ),
    (   get_dict(completion_tokens_details, Usage0, CompletionDetails),
        get_dict(reasoning_tokens, CompletionDetails, Reasoning0)
    ->  true
    ;   Reasoning0 = 0
    ),
    integer_or(Input0, 0, Input),
    integer_or(Output0, 0, Output),
    integer_or(Total0, Input + Output, Total),
    integer_or(CacheRead0, 0, CacheRead),
    integer_or(Reasoning0, 0, Reasoning),
    CacheWrite = 0.

integer_or(Value, _, Value) :- integer(Value), !.
integer_or(_, Default, Default).

%% decode_json(+Json, :Callback, -Response)
decode_json(Json, Callback, Response) :-
    (   get_dict(choices, Json, [Choice|_])
    ->  (   get_dict(message, Choice, Message)
        ->  get_dict(content, Message, Content0),
            ( Content0 == null -> Text = "" ; dc_json:as_text(Content0, Text) ),
            ( get_dict(tool_calls, Message, ToolCalls0), is_list(ToolCalls0)
            ->  decode_tool_calls(ToolCalls0, ToolCalls)
            ;   ToolCalls = []
            ),
            ( get_dict(usage, Json, Usage0), is_dict(Usage0)
            ->  normalize_usage(Usage0, Usage)
            ;   Usage = usage{input: 0, output: 0, total: 0, cache_read: 0, cache_write: 0, reasoning: 0}
            ),
            get_dict(finish_reason, Choice, Finish0),
            finish_reason(Finish0, ToolCalls, Finish),
            Response = llm_response{text: Text, reasoning: "", tool_calls: ToolCalls,
                                    usage: Usage, finish_reason: Finish, raw: none},
            ( Text \== "" -> dc_llm:emit_event(Callback, text(Text)) ; true )
        ;   Response = llm_response{text: "", reasoning: "", tool_calls: [],
                                    usage: usage{input:0, output:0, total:0, cache_read:0, cache_write:0, reasoning:0},
                                    finish_reason: stop, raw: none}
        )
    ;   Response = llm_response{text: "", reasoning: "", tool_calls: [],
                                usage: usage{input:0, output:0, total:0, cache_read:0, cache_write:0, reasoning:0},
                                finish_reason: stop, raw: none}
    ).

decode_tool_calls([], []).
decode_tool_calls([Call|Calls], [tool_call{id: Id, name: Name, arguments: Arguments}|Out]) :-
    ( get_dict(id, Call, Id0) -> dc_json:as_text(Id0, Id) ; Id = "" ),
    ( get_dict(function, Call, Function) -> true ; Function = _{} ),
    ( get_dict(name, Function, Name0) -> dc_json:as_text(Name0, Name) ; Name = "" ),
    ( get_dict(arguments, Function, Args0) -> dc_json:as_text(Args0, ArgsText) ; ArgsText = "" ),
    parse_arguments(ArgsText, Arguments),
    decode_tool_calls(Calls, Out).


%% openai_headers(+Provider, -Headers)
%% Local OpenAI-compatible servers often need no key; hosted providers do.
openai_headers(Provider, Headers) :-
    (   Provider == openai_compatible
    ->  (   catch(dc_config:api_key(Provider, ApiKey), _, fail)
        ->  atomic_list_concat(['Bearer ', ApiKey], Header),
            Headers = [('Authorization'-Header)]
        ;   Headers = []
        )
    ;   dc_config:api_key(Provider, ApiKey),
        (   Provider == azure
        ->  Headers = [('api-key'-ApiKey)]
        ;   atomic_list_concat(['Bearer ', ApiKey], Header),
            Headers = [('Authorization'-Header)]
        )
    ).


%% openai_url(+Provider, +Base, -URL)
%% Azure deployments need their own endpoint/query; OpenAI-compatible servers
%% may already be configured with the full path.
openai_url(Provider, Base, URL) :-
    dc_json:as_text(Base, BaseText),
    (   sub_string(BaseText, _, _, _, "/chat/completions")
    ->  URL = BaseText
    ;   Provider == azure
    ->  (   catch(dc_config:setting([providers, azure, api_version], Version0), _, fail),
            dc_json:as_text(Version0, Version),
            Version \== ""
        ->  atomic_list_concat([BaseText, '/chat/completions?api-version=', Version], URL)
        ;   atomic_list_concat([BaseText, '/chat/completions'], URL)
        )
    ;   atomic_list_concat([BaseText, '/chat/completions'], URL)
    ).
