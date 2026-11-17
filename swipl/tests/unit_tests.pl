%% Unit tests for the plain runtime. Run from the repository root:
%%   swipl -q -g "run_tests,halt" swipl/tests/unit_tests.pl
:- initialization(main, main).
:- use_module(library(plunit)).
:- prolog_load_context(directory, Here),
   atomic_list_concat([Here, '/../prolog'], P),
   use_module(P/dc_json),
   use_module(P/dc_config),
   use_module(P/dc_sse),
   use_module(P/dc_llm),
   use_module(P/dc_agent),
   use_module(P/dc_tools),
   use_module(P/providers/dc_llm_openai, []),
   use_module(P/providers/dc_llm_anthropic, []),
   use_module(P/providers/dc_llm_google, []).

initial_state(S) :-
    S = llm_state{text:"", reasoning:"", tool_calls:[],
                  usage:usage{input:0, output:0, total:0,
                              cache_read:0, cache_write:0, reasoning:0},
                  finish_reason:none, raw:none}.

main :- run_tests.

collect_sse(Event, Data) :-
    assertz(user:sse_event(Event, Data)).

:- begin_tests(plain_runtime).

test(json_roundtrip) :-
    dc_json:json_text(_{a:1, b:[2,3]}, Text),
    dc_json:json_parse(Text, Parsed),
    get_dict(a, Parsed, 1),
    get_dict(b, Parsed, [2,3]).

test(tool_schema_from_mi) :-
    T = tool_info{name: echo,
                  schema: schema{name: echo, arity: 2, description: "Echo",
                                 inputs: [param{name: arg1, type: string,
                                                description: "Text"}],
                                 outputs: [param{name: arg2, type: string,
                                                 description: "Out"}]},
                  source: ""},
    dc_json:tool_json_schema(T, Schema),
    get_dict(type, Schema, object),
    get_dict(properties, Schema, Props),
    get_dict(arg1, Props, Arg),
    get_dict(type, Arg, string).

test(sse_events) :-
    open_string("event: delta\ndata: one\ndata: two\n\ndata: three\n\n", Stream),
    dc_sse:stream_events(Stream, user:collect_sse),
    close(Stream),
    findall(Event-Data, retract(user:sse_event(Event, Data)), Events),
    Events == ["delta"-'one\ntwo', "message"-'three'].

test(openai_stream) :-
    initial_state(S0),
    dc_llm_openai:event(message,
        '{"choices":[{"delta":{"content":"Hi"}}]}', _{}, S0, S1, none),
    dc_llm_openai:event(message,
        '{"choices":[{"delta":{"tool_calls":[{"index":0,"id":"c1","type":"function","function":{"name":"echo","arguments":"{\\"a\\":1}"}}]}}]}',
        _{}, S1, S2, none),
    dc_llm_openai:event(message,
        '{"choices":[{"delta":{},"finish_reason":"tool_calls"}]}', _{}, S2, S3, none),
    dc_llm_openai:finish(S3, R),
    get_dict(text, R, "Hi"),
    get_dict(tool_calls, R, OpenAICalls),
    OpenAICalls = [OpenAICall],
    get_dict(id, OpenAICall, "c1"),
    get_dict(name, OpenAICall, "echo"),
    get_dict(arguments, OpenAICall, A),
    get_dict(a, A, 1), !.

test(anthropic_stream) :-
    initial_state(S0),
    dc_llm_anthropic:event(content_block_delta,
        '{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Hi"}}',
        _{}, S0, S1, none),
    dc_llm_anthropic:event(content_block_start,
        '{"type":"content_block_start","index":1,"content_block":{"type":"tool_use","id":"t1","name":"echo"}}',
        _{}, S1, S2, none),
    dc_llm_anthropic:event(content_block_delta,
        '{"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":"{\\"a\\":1}"}}',
        _{}, S2, S3, none),
    dc_llm_anthropic:event(message_delta,
        '{"type":"message_delta","delta":{"stop_reason":"tool_use"},"usage":{"output_tokens":3}}',
        _{}, S3, S4, none),
    dc_llm_anthropic:finish(S4, R),
    get_dict(text, R, "Hi"),
    get_dict(tool_calls, R, AnthCalls),
    AnthCalls = [AnthCall],
    get_dict(id, AnthCall, "t1"),
    get_dict(name, AnthCall, "echo"),
    get_dict(arguments, AnthCall, A),
    get_dict(a, A, 1), !.

test(google_stream) :-
    initial_state(S0),
    dc_llm_google:event(message,
        '{"candidates":[{"content":{"parts":[{"text":"Hey"}]}}]}',
        _{}, S0, S1, none),
    dc_llm_google:event(message,
        '{"candidates":[{"content":{"parts":[{"functionCall":{"name":"echo","args":{"a":1}}}]},"finishReason":"STOP"}]}',
        _{}, S1, S2, none),
    dc_llm_google:finish(S2, R),
    get_dict(text, R, "Hey"),
    get_dict(tool_calls, R, GoogleCalls),
    GoogleCalls = [GoogleCall],
    get_dict(name, GoogleCall, "echo"),
    get_dict(arguments, GoogleCall, A),
    get_dict(a, A, 1), !.

test(openai_request) :-
    setenv('OPENAI_API_KEY', 'test'),
    dc_config:init([provider-openai, model-'gpt-4o-mini']),
    dc_llm:request_from_messages([llm_message{role:user, content:"hi"}], [], R),
    dc_llm_openai:request(R, H),
    get_dict(url, H, URL),
    dc_json:as_text(URL, URLText),
    sub_string(URLText, _, _, _, "/chat/completions"),
    get_dict(body, H, Body),
    get_dict(model, Body, Model0),
    dc_json:as_text(Model0, "gpt-4o-mini"),
    get_dict(stream, Body, true), !.


test(anthropic_request) :-
    setenv('ANTHROPIC_API_KEY', 'test'),
    dc_config:init([provider-anthropic, model-'claude-test']),
    dc_llm:request_from_messages(
        [message{role:system, content:"sys"}, message{role:user, content:"hi"}], [], R),
    dc_llm_anthropic:request(R, H),
    get_dict(url, H, URL0), dc_json:as_text(URL0, URL),
    sub_string(URL, _, _, _, "/v1/messages"),
    get_dict(body, H, Body),
    get_dict(system, Body, "sys"),
    get_dict(stream, Body, true), !.

test(google_request) :-
    setenv('GEMINI_API_KEY', 'test'),
    dc_config:init([provider-google, model-'gemini-test']),
    dc_llm:request_from_messages([message{role:user, content:"hi"}], [], R),
    dc_llm_google:request(R, H),
    get_dict(url, H, URL0), dc_json:as_text(URL0, URL),
    sub_string(URL, _, _, _, ":streamGenerateContent"),
    get_dict(body, H, Body),
    get_dict(contents, Body, Contents),
    Contents = [Content],
    get_dict(role, Content, user), !.

test(agent_tool_arguments) :-
    T = tool_info{name: echo,
                  schema: schema{name: echo, arity: 2, description: "Echo",
                                 inputs: [param{name: arg1, type: string,
                                                description: "Text"}],
                                 outputs: [param{name: arg2, type: string,
                                                 description: "Out"}]},
                  source: ""},
    (   dc_agent:tool_info_name(T, echo) -> true ; fail ),
    dc_agent:tool_args_positional(T, _{arg1:"hi"}, Args),
    Args == ["hi"].

:- end_tests(plain_runtime).
