%% Anthropic Messages adapter.
:- module(dc_llm_anthropic, [request/2, event/6, finish/2, decode_json/3]).

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
    anthropic_url(Base, URL),
    split_system(Messages, System, Conversation),
    anthropic_messages(Conversation, AnthropicMessages),
    anthropic_tools(Tools, AnthropicTools),
    Body0 = _{model: Model, max_tokens: MaxTokens, messages: AnthropicMessages, stream: true},
    (   System == "" -> Body1 = Body0 ; put_dict(system, Body0, System, Body1) ),
    (   number(Temperature) -> put_dict(temperature, Body1, Temperature, Body2) ; Body2 = Body1 ),
    (   AnthropicTools == [] -> Body = Body2 ; put_dict(tools, Body2, AnthropicTools, Body) ),
    dc_config:api_key(Provider, ApiKey),
    Headers = [('x-api-key'-ApiKey), ('anthropic-version'-"2023-06-01")],
    Http = http_request{url: URL, method: post, headers: Headers,
                        body: Body, timeout: 120}.

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
        split_system(Messages, RestSystem, Conversation),
        (   RestSystem == ""
        ->  System = Text
        ;   string_concat(Text, "\n\n", Joined),
            string_concat(Joined, RestSystem, System)
        )
    ;   split_system(Messages, System, RestConversation),
        Conversation = [Message|RestConversation]
    ).

message_role(Message, Role) :-
    ( is_dict(Message), get_dict(role, Message, Role0) -> dc_json:key_atom(Role0, Role) ; Role = user ).
message_content_text(Message, Text) :-
    ( is_dict(Message), get_dict(content, Message, Content0) -> dc_json:as_text(Content0, Text) ; Text = "" ).

%% Merge adjacent same-role messages so the provider sees the alternating
%% structure it expects. Each message becomes a list of content blocks.
anthropic_messages(Messages, Out) :-
    fold_messages(Messages, [], RevPairs),
    reverse(RevPairs, Pairs),
    pairs_to_messages(Pairs, Out).

fold_messages([], Acc, Acc).
fold_messages([Message|Messages], Acc, Out) :-
    message_role(Message, Role0),
    (   Role0 == tool -> Role = user ; Role = Role0 ),
    anthropic_blocks(Message, Blocks),
    (   Acc = [Role-Previous|Rest]
    ->  append(Previous, Blocks, Merged),
        fold_messages(Messages, [Role-Merged|Rest], Out)
    ;   fold_messages(Messages, [Role-Blocks|Acc], Out)
    ).

pairs_to_messages([], []).
pairs_to_messages([Role-Blocks|Pairs], [_{role: Role, content: Blocks}|Out]) :-
    pairs_to_messages(Pairs, Out).

anthropic_blocks(Message, Blocks) :-
    message_role(Message, Role),
    (   Role == tool
    ->  message_tool_call_id(Message, Id),
        message_content_text(Message, Text),
        Blocks = [_{type: tool_result, tool_use_id: Id, content: Text}]
    ;   message_tool_calls(Message, Calls),
        Calls \== []
    ->  message_content_text(Message, Text),
        (   Text == ""
        ->  TextBlocks = []
        ;   TextBlocks = [_{type: text, text: Text}]
        ),
        anthropic_tool_use_blocks(Calls, ToolBlocks),
        append(TextBlocks, ToolBlocks, Blocks)
    ;   message_content_text(Message, Text),
        Blocks = [_{type: text, text: Text}]
    ).

message_tool_calls(Message, Calls) :-
    ( is_dict(Message), get_dict(tool_calls, Message, Calls0), is_list(Calls0) -> Calls = Calls0 ; Calls = [] ).
message_tool_call_id(Message, Id) :-
    ( is_dict(Message), get_dict(tool_call_id, Message, Id0) -> dc_json:as_text(Id0, Id) ; Id = "" ).

anthropic_tool_use_blocks([], []).
anthropic_tool_use_blocks([Call|Calls], [_{type: tool_use, id: Id, name: Name, input: Input}|Out]) :-
    call_field(Call, id, "", Id),
    call_field(Call, name, "", Name),
    ( is_dict(Call), get_dict(arguments, Call, Arguments), is_dict(Arguments) -> Input = Arguments ; Input = _{} ),
    anthropic_tool_use_blocks(Calls, Out).

call_field(Call, Key, Default, Value) :-
    ( is_dict(Call), get_dict(Key, Call, Value0) -> dc_json:as_text(Value0, Value) ; Value = Default ).

anthropic_tools([], []).
anthropic_tools([Tool|Tools], [Out|Outs]) :-
    tool_field(Tool, name, Name),
    tool_field(Tool, description, Description),
    ( is_dict(Tool), get_dict(parameters, Tool, Parameters) -> true ; Parameters = _{} ),
    Out = _{name: Name, description: Description, input_schema: Parameters},
    anthropic_tools(Tools, Outs).

tool_field(Tool, Key, Value) :-
    ( is_dict(Tool), get_dict(Key, Tool, Value0) -> dc_json:as_text(Value0, Value) ; Value = "" ).

%% event(+EventName, +Data, +Request, +State0, -State, :Callback)
event(_, Data, _Request, State0, State, Callback) :-
    catch(
        (   dc_json:json_parse(Data, Json),
            anthropic_event(Json, State0, State, Callback)
        ),
        Error,
        (   format(string(Message), "Anthropic stream parse failed: ~w", [Error]),
            dc_llm:emit_event(Callback, error(Message)),
            State = State0
        )
    ).

anthropic_event(Json, State0, State, Callback) :-
    (   get_dict(type, Json, Type0) -> dc_json:key_atom(Type0, Type) ; Type = none ),
    anthropic_event_type(Type, Json, State0, State, Callback).

anthropic_event_type(message_start, Json, State0, State, _Callback) :-
    (   get_dict(message, Json, Message),
        get_dict(usage, Message, Usage0),
        get_dict(input_tokens, Usage0, Input0)
    ->  integer_or(Input0, 0, Input),
        update_usage(State0, input, Input, State)
    ;   State = State0
    ).
anthropic_event_type(content_block_start, Json, State0, State, _Callback) :-
    (   get_dict(index, Json, Index),
        get_dict(content_block, Json, Block),
        get_dict(type, Block, Type0),
        dc_json:key_atom(Type0, Type),
        Type == tool_use
    ->  ( get_dict(id, Block, Id0) -> dc_json:as_text(Id0, Id) ; Id = "" ),
        ( get_dict(name, Block, Name0) -> dc_json:as_text(Name0, Name) ; Name = "" ),
        get_dict(tool_calls, State0, Existing),
        put_tool_call(Existing, Index, Id, Name, "", Updated),
        put_dict(tool_calls, State0, Updated, State)
    ;   State = State0
    ).
anthropic_event_type(content_block_delta, Json, State0, State, Callback) :-
    (   get_dict(index, Json, Index),
        get_dict(delta, Json, Delta),
        get_dict(type, Delta, DeltaType0),
        dc_json:key_atom(DeltaType0, DeltaType)
    ->  (   DeltaType == text_delta
        ->  get_dict(text, Delta, Text0),
            dc_json:as_text(Text0, Text),
            append_state_text(State0, text, Text, State1),
            dc_llm:emit_event(Callback, text(Text))
        ;   DeltaType == thinking_delta
        ->  get_dict(thinking, Delta, Text0),
            dc_json:as_text(Text0, Text),
            append_state_text(State0, reasoning, Text, State1),
            dc_llm:emit_event(Callback, reasoning(Text))
        ;   DeltaType == input_json_delta
        ->  get_dict(partial_json, Delta, Text0),
            dc_json:as_text(Text0, Text),
            get_dict(tool_calls, State0, Calls0),
            append_tool_call_args(Calls0, Index, Text, Calls1),
            put_dict(tool_calls, State0, Calls1, State1)
        ;   State1 = State0
        ),
        State = State1
    ;   State = State0
    ).
anthropic_event_type(message_delta, Json, State0, State, _Callback) :-
    (   get_dict(delta, Json, Delta),
        get_dict(stop_reason, Delta, Stop0),
        Stop0 \== null
    ->  put_dict(finish_reason, State0, Stop0, State1)
    ;   State1 = State0
    ),
    (   get_dict(usage, Json, Usage0),
        get_dict(output_tokens, Usage0, Output0)
    ->  integer_or(Output0, 0, Output),
        update_usage(State1, output, Output, State)
    ;   State = State1
    ).
anthropic_event_type(error, Json, State0, State, Callback) :-
    (   get_dict(error, Json, Error)
    ->  format(string(Message), "Anthropic error: ~w", [Error]),
        dc_llm:emit_event(Callback, error(Message))
    ;   true
    ),
    State = State0.
anthropic_event_type(_, _, State, State, _).

update_usage(State0, Kind, Value, State) :-
    get_dict(usage, State0, Usage0),
    (   Kind == input
    ->  Usage = Usage0.put(input, Value).put(total, Value)
    ;   get_dict(input, Usage0, Input),
        Total is Input + Value,
        Usage = Usage0.put(output, Value).put(total, Total)
    ),
    put_dict(usage, State0, Usage, State).

append_state_text(State0, Key, Text, State) :-
    get_dict(Key, State0, Old0),
    dc_json:as_text(Old0, Old),
    string_concat(Old, Text, New),
    put_dict(Key, State0, New, State).

put_tool_call([], Index, Id, Name, Args, [tc{index: Index, id: Id, name: Name, args: Args}]).
put_tool_call([tc{index: I, id: Id0, name: Name0, args: Args0}|Rest], Index, Id, Name, Args,
              [tc{index: Index, id: Id1, name: Name1, args: Args1}|Rest]) :-
    I == Index,
    !,
    ( Id == "" -> Id1 = Id0 ; Id1 = Id ),
    ( Name == "" -> Name1 = Name0 ; Name1 = Name ),
    string_concat(Args0, Args, Args1).
put_tool_call([Call|Rest], Index, Id, Name, Args, [Call|Updated]) :-
    put_tool_call(Rest, Index, Id, Name, Args, Updated).

append_tool_call_args([], _, _, []).
append_tool_call_args([tc{index: I, id: Id, name: Name, args: Args0}|Rest], Index, Text,
                      [tc{index: I, id: Id, name: Name, args: Args1}|Rest]) :-
    I == Index,
    !,
    string_concat(Args0, Text, Args1).
append_tool_call_args([Call|Rest], Index, Text, [Call|Updated]) :-
    append_tool_call_args(Rest, Index, Text, Updated).

finish(State, Response) :-
    get_dict(text, State, Text),
    get_dict(reasoning, State, Reasoning),
    get_dict(tool_calls, State, Calls0),
    predsort(compare_tc, Calls0, Calls1),
    finalize_tool_calls(Calls1, Calls),
    get_dict(usage, State, Usage),
    get_dict(finish_reason, State, Finish0),
    finish_reason(Finish0, Calls, Finish),
    Response = llm_response{text: Text, reasoning: Reasoning, tool_calls: Calls,
                            usage: Usage, finish_reason: Finish, raw: none}.

compare_tc(Order, tc{index: A}, tc{index: B}) :- compare(Order, A, B).

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
    ;   catch(dc_json:json_parse(Text, Parsed), _, fail),
        ( is_dict(Parsed) -> Arguments = Parsed ; Arguments = _{value: Parsed} )
    ->  true
    ;   Arguments = _{}
    ).

finish_reason(_, Calls, tool_use) :- Calls \== [], !.
finish_reason(none, _, stop) :- !.
finish_reason(Reason0, _, Reason) :- dc_json:key_atom(Reason0, Reason).

integer_or(Value, _, Value) :- integer(Value), !.
integer_or(_, Default, Default).

decode_json(Json, Callback, Response) :-
    (   get_dict(content, Json, Content), is_list(Content)
    ->  decode_blocks(Content, Text, Reasoning, ToolCalls),
        ( get_dict(usage, Json, Usage0), is_dict(Usage0) -> decode_usage(Usage0, Usage)
        ; Usage = usage{input:0, output:0, total:0, cache_read:0, cache_write:0, reasoning:0} ),
        ( get_dict(stop_reason, Json, Finish0) -> finish_reason(Finish0, ToolCalls, Finish) ; Finish = stop ),
        Response = llm_response{text: Text, reasoning: Reasoning, tool_calls: ToolCalls,
                                usage: Usage, finish_reason: Finish, raw: none},
        ( Text \== "" -> dc_llm:emit_event(Callback, text(Text)) ; true ),
        ( Reasoning \== "" -> dc_llm:emit_event(Callback, reasoning(Reasoning)) ; true )
    ;   Response = llm_response{text: "", reasoning: "", tool_calls: [],
                                usage: usage{input:0, output:0, total:0, cache_read:0, cache_write:0, reasoning:0},
                                finish_reason: stop, raw: none}
    ).

decode_blocks([], "", "", []).
decode_blocks([Block|Blocks], Text, Reasoning, ToolCalls) :-
    get_dict(type, Block, Type0),
    dc_json:key_atom(Type0, Type),
    decode_blocks(Blocks, Text0, Reasoning0, ToolCalls0),
    (   Type == text
    ->  get_dict(text, Block, T), dc_json:as_text(T, Text), Reasoning = Reasoning0, ToolCalls = ToolCalls0
    ;   Type == thinking
    ->  get_dict(thinking, Block, T), dc_json:as_text(T, Reasoning), Text = Text0, ToolCalls = ToolCalls0
    ;   Type == tool_use
    ->  ( get_dict(id, Block, Id0) -> dc_json:as_text(Id0, Id) ; Id = "" ),
        ( get_dict(name, Block, Name0) -> dc_json:as_text(Name0, Name) ; Name = "" ),
        ( get_dict(input, Block, Input), is_dict(Input) -> Arguments = Input ; Arguments = _{} ),
        Text = Text0, Reasoning = Reasoning0,
        ToolCalls = [tool_call{id: Id, name: Name, arguments: Arguments}|ToolCalls0]
    ;   Text = Text0, Reasoning = Reasoning0, ToolCalls = ToolCalls0
    ).

decode_usage(Usage0, usage{input: Input, output: Output, total: Total,
                           cache_read: CacheRead, cache_write: CacheWrite, reasoning: 0}) :-
    ( get_dict(input_tokens, Usage0, Input0) -> integer_or(Input0, 0, Input) ; Input = 0 ),
    ( get_dict(output_tokens, Usage0, Output0) -> integer_or(Output0, 0, Output) ; Output = 0 ),
    ( get_dict(cache_read_input_tokens, Usage0, CR0) -> integer_or(CR0, 0, CacheRead) ; CacheRead = 0 ),
    ( get_dict(cache_creation_input_tokens, Usage0, CW0) -> integer_or(CW0, 0, CacheWrite) ; CacheWrite = 0 ),
    Total is Input + Output.


anthropic_url(Base, URL) :-
    dc_json:as_text(Base, BaseText),
    (   sub_string(BaseText, _, _, _, "/messages")
    ->  URL = BaseText
    ;   sub_string(BaseText, _, _, 0, "/v1")
    ->  atomic_list_concat([BaseText, '/messages'], URL)
    ;   atomic_list_concat([BaseText, '/v1/messages'], URL)
    ).
