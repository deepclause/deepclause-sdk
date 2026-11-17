%% Runtime configuration for the plain SWI DeepClause host.
:- module(dc_config, [
    init/0,
    init/1,
    config/1,
    workspace/1,
    set_workspace/1,
    provider/1,
    set_provider/1,
    model/1,
    set_model/1,
    temperature/1,
    max_tokens/1,
    max_model_calls/1,
    timeout/1,
    api_key/2,
    api_key_env/2,
    base_url/2,
    provider_adapter/2,
    provider_kind/2,
    default_model/2,
    model_context/2,
    setting/2
]).

:- use_module(library(filesex)).
:- use_module(library(http/json)).
:- use_module(dc_json).

:- dynamic config_dict/1.
:- dynamic current_workspace/1.
:- dynamic current_provider/1.
:- dynamic current_model/1.

%% init/0 uses the current directory and environment.
init :- init([]).

%% init(+Options)
%% Options: provider(P), model(M), workspace(W), config(Path)
init(Options) :-
    retractall(config_dict(_)),
    retractall(current_workspace(_)),
    retractall(current_provider(_)),
    retractall(current_model(_)),
    option_workspace(Options, Workspace0),
    (   exists_directory(Workspace0)
    ->  true
    ;   make_directory_path(Workspace0)
    ),
    absolute_file_name(Workspace0, Workspace, [file_type(directory), access(read)]),
    assertz(current_workspace(Workspace)),
    load_config(Options, Config),
    assertz(config_dict(Config)),
    (   option_value(Options, provider, EnvProvider)
    ->  Provider0 = EnvProvider
    ;   config_path_value(Config, [llm, provider], "", ConfigProvider),
        (   ConfigProvider == ""
        ->  DefaultProvider = "openai"
        ;   DefaultProvider = ConfigProvider
        ),
        env_or_default('DC_PROVIDER', Provider0, DefaultProvider)
    ),
    dc_json:as_atom(Provider0, Provider),
    assertz(current_provider(Provider)),
    (   option_value(Options, model, OptionModel)
    ->  Model = OptionModel
    ;   config_path_value(Config, [llm, model], Model0),
        env_or_default('DC_MODEL', EnvModel, Model0),
        default_model(Provider, DefaultModel),
        (   EnvModel == "" -> Model = DefaultModel ; Model = EnvModel )
    ),
    assertz(current_model(Model)).

option_workspace(Options, Workspace) :-
    (   option_value(Options, workspace, W), nonvar(W)
    ->  Workspace = W
    ;   env_value('DC_WORKSPACE', W2),
        W2 \== ""
    ->  Workspace = W2
    ;   Workspace = '.'
    ).

option_value([K-V|_], K, V) :- !.
option_value([_|T], K, V) :- option_value(T, K, V).

load_config(Options, Config) :-
    (   option_value(Options, config, Explicit)
    ->  true
    ;   Explicit = none
    ),
    (   Explicit \== none
    ->  Path = Explicit
    ;   env_value('DC_CONFIG', EnvPath),
        EnvPath \== ""
    ->  Path = EnvPath
    ;   workspace(Workspace),
        directory_file_path(Workspace, '.deepclause/config.json', Local),
        (   exists_file(Local)
        ->  Path = Local
        ;   expand_file_name('~/.config/deepclause/config.json', [Home]),
            (   exists_file(Home) -> Path = Home ; Path = none )
        )
    ),
    (   Path == none
    ->  Config = _{}
    ;   exists_file(Path)
    ->  catch(
            ( read_file_to_string(Path, Text, [encoding(utf8)]),
              dc_json:json_parse(Text, Parsed),
              ( is_dict(Parsed) -> Config = Parsed ; Config = _{} ) ),
            _,
            Config = _{}
        )
    ;   Config = _{}
    ).

config(Config) :- config_dict(Config).

workspace(Workspace) :- current_workspace(Workspace).

set_workspace(Workspace) :-
    retractall(current_workspace(_)),
    assertz(current_workspace(Workspace)).

provider(Provider) :- current_provider(Provider).

set_provider(Provider0) :-
    dc_json:as_atom(Provider0, Provider),
    retractall(current_provider(_)),
    assertz(current_provider(Provider)),
    retractall(current_model(_)),
    default_model(Provider, Model),
    assertz(current_model(Model)).

model(Model) :- current_model(Model).

set_model(Model) :-
    retractall(current_model(_)),
    assertz(current_model(Model)).

temperature(Temperature) :-
    config_dict(Config),
    config_path_value(Config, [llm, temperature], 0.2, Temperature0),
    number_or(Temperature0, 0.2, Temperature),
    !.

max_tokens(MaxTokens) :-
    config_dict(Config),
    config_path_value(Config, [llm, max_tokens], 4096, Value),
    integer_or(Value, 4096, MaxTokens),
    !.

max_model_calls(MaxCalls) :-
    config_dict(Config),
    config_path_value(Config, [repl, max_model_calls], 24, Value),
    integer_or(Value, 24, MaxCalls),
    !.

timeout(Timeout) :-
    config_dict(Config),
    config_path_value(Config, [llm, timeout], 120, Value),
    integer_or(Value, 120, Timeout),
    !.

%% setting(+Key, -Value) is a small convenience for the REPL.
setting(Key, Value) :-
    config_dict(Config),
    config_path_value(Config, Key, "", Value).

%% api_key_env(+Provider, -Env)
api_key_env(Provider, Env) :-
    config_dict(Config),
    config_path_value(Config, [providers, Provider, api_key_env], Default, Env0),
    default_api_key_env(Provider, Default),
    (   Env0 == "" -> Env = Default ; Env = Env0 ).

%% api_key(+Provider, -Key)
api_key(Provider, Key) :-
    api_key_env(Provider, Env),
    env_value(Env, Key0),
    (   Key0 \== ""
    ->  Key = Key0
    ;   config_dict(Config),
        config_path_value(Config, [providers, Provider, api_key], "", Configured),
        (   Configured \== "" -> Key = Configured
        ;   config_path_value(Config, [llm, api_key], "", Key)
        )
    ),
    (   Key == ""
    ->  throw(error(llm_config(no_api_key(Provider, Env)), api_key/2))
    ;   true
    ),
    !.

default_api_key_env(openai, 'OPENAI_API_KEY').
default_api_key_env(anthropic, 'ANTHROPIC_API_KEY').
default_api_key_env(google, 'GEMINI_API_KEY').
default_api_key_env(gemini, 'GEMINI_API_KEY').
default_api_key_env(openrouter, 'OPENROUTER_API_KEY').
default_api_key_env(azure, 'AZURE_OPENAI_API_KEY').
default_api_key_env(groq, 'GROQ_API_KEY').
default_api_key_env(mistral, 'MISTRAL_API_KEY').
default_api_key_env(deepseek, 'DEEPSEEK_API_KEY').
default_api_key_env(xai, 'XAI_API_KEY').
default_api_key_env(together, 'TOGETHER_API_KEY').
default_api_key_env(fireworks, 'FIREWORKS_API_KEY').
default_api_key_env(cerebras, 'CEREBRAS_API_KEY').
default_api_key_env(nvidia, 'NVIDIA_API_KEY').
default_api_key_env(openai_compatible, 'OPENAI_COMPATIBLE_API_KEY').
default_api_key_env(_, 'OPENAI_API_KEY').

%% base_url(+Provider, -URL)
base_url(azure, URL) :-
    !,
    config_dict(Config),
    config_path_value(Config, [providers, azure, base_url], "", URL0),
    (   URL0 == ""
    ->  throw(error(llm_config(azure_base_url_required), base_url/2))
    ;   URL = URL0
    ).

base_url(Provider, URL) :-
    config_dict(Config),
    config_path_value(Config, [providers, Provider, base_url], "", Configured),
    (   Configured \== ""
    ->  URL = Configured
    ;   config_path_value(Config, [llm, base_url], "", Global),
        (   Global \== ""
        ->  URL = Global
        ;   default_base_url(Provider, URL)
        )
    ),
    !.

default_base_url(openai, "https://api.openai.com/v1").
default_base_url(openrouter, "https://openrouter.ai/api/v1").
default_base_url(anthropic, "https://api.anthropic.com").
default_base_url(google, "https://generativelanguage.googleapis.com/v1beta").
default_base_url(gemini, "https://generativelanguage.googleapis.com/v1beta").
default_base_url(groq, "https://api.groq.com/openai/v1").
default_base_url(mistral, "https://api.mistral.ai/v1").
default_base_url(deepseek, "https://api.deepseek.com").
default_base_url(xai, "https://api.x.ai/v1").
default_base_url(together, "https://api.together.ai/v1").
default_base_url(fireworks, "https://api.fireworks.ai/inference/v1").
default_base_url(cerebras, "https://api.cerebras.ai/v1").
default_base_url(nvidia, "https://integrate.api.nvidia.com/v1").
default_base_url(openai_compatible, "http://127.0.0.1:11434/v1").
default_base_url(_, "https://api.openai.com/v1").

%% provider_adapter(+Provider, -Module)
provider_adapter(openai, dc_llm_openai) :- !.
provider_adapter(openrouter, dc_llm_openai) :- !.
provider_adapter(azure, dc_llm_openai) :- !.
provider_adapter(groq, dc_llm_openai) :- !.
provider_adapter(mistral, dc_llm_openai) :- !.
provider_adapter(deepseek, dc_llm_openai) :- !.
provider_adapter(xai, dc_llm_openai) :- !.
provider_adapter(together, dc_llm_openai) :- !.
provider_adapter(fireworks, dc_llm_openai) :- !.
provider_adapter(cerebras, dc_llm_openai) :- !.
provider_adapter(nvidia, dc_llm_openai) :- !.
provider_adapter(openai_compatible, dc_llm_openai) :- !.
provider_adapter(anthropic, dc_llm_anthropic) :- !.
provider_adapter(google, dc_llm_google) :- !.
provider_adapter(gemini, dc_llm_google) :- !.
provider_adapter(Provider0, Module) :-
    (   string(Provider0)
    ->  atom_string(Provider, Provider0),
        provider_adapter(Provider, Module)
    ;   throw(error(llm_config(unknown_provider(Provider0)), provider_adapter/2))
    ).

%% provider_kind(+Provider, -Kind)
provider_kind(Provider, openai) :- provider_adapter(Provider, dc_llm_openai), !.
provider_kind(Provider, anthropic) :- provider_adapter(Provider, dc_llm_anthropic), !.
provider_kind(Provider, google) :- provider_adapter(Provider, dc_llm_google), !.
provider_kind(_, openai).

default_model(openai, "gpt-4o-mini").
default_model(openrouter, "openai/gpt-4o-mini").
default_model(anthropic, "claude-3-5-haiku-latest").
default_model(google, "gemini-2.0-flash").
default_model(gemini, "gemini-2.0-flash").
default_model(groq, "llama-3.3-70b-versatile").
default_model(mistral, "mistral-small-latest").
default_model(deepseek, "deepseek-chat").
default_model(xai, "grok-2-latest").
default_model(together, "meta-llama/Llama-3.3-70B-Instruct-Turbo").
default_model(fireworks, "accounts/fireworks/models/llama-v3p3-70b-instruct").
default_model(cerebras, "llama-3.3-70b").
default_model(nvidia, "meta/llama-3.3-70b-instruct").
default_model(openai_compatible, "local-model").
default_model(_, "gpt-4o-mini").

%% model_context(+Model, -Tokens). A conservative fallback for context checks.
model_context(Model, Context) :-
    config_dict(Config),
    config_path_value(Config, [models, Model, context], 128000, Value),
    integer_or(Value, 128000, Context).

config_path_value(Config, Path, Value) :-
    config_path_value(Config, Path, "", Value).

config_path_value(Config, Path, Default, Value) :-
    config_path_value(Config, Path, Default, Value, true).

config_path_value(Value, [], _, Value, _) :- !.
config_path_value(Config, [Key|Keys], Default, Value, First) :-
    (   First == true
    ->  key_atom_local(Key, Atom)
    ;   key_atom_local(Key, Atom)
    ),
    (   is_dict(Config),
        get_dict(Atom, Config, Next)
    ->  config_path_value(Next, Keys, Default, Value, false)
    ;   Value = Default
    ).

key_atom_local(Key, Atom) :-
    (   atom(Key) -> Atom = Key
    ;   string(Key) -> atom_string(Atom, Key)
    ;   Atom = Key
    ).

integer_or(Value, _, Value) :- integer(Value), !.
integer_or(_, Default, Default).

number_or(Value, _, Value) :- number(Value), !.
number_or(_, Default, Default).

env_value(Name, Value) :-
    (   atom(Name) -> atom_string(Name, S) ; S = Name ),
    (   getenv(S, V)
    ->  Value = V
    ;   Value = ""
    ).

env_or_default(Name, Value, Default) :-
    env_value(Name, EnvValue),
    (   EnvValue == ""
    ->  Value = Default
    ;   Value = EnvValue
    ).
