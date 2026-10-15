/**
 * deepclause_mi.pl - Meta-interpreter for DeepClause SDK
 * 
 * The simplified meta-interpreter that handles:
 * - task/1 and task/N predicates (agent loops)
 * - exec/2 predicate (external tool calls)
 * - Memory predicates (system, user) - now backtrackable via state
 * - Output predicates (answer, yield, log)
 * - Parameter handling
 * 
 * Key design decisions:
 * - No @-predicates - all LLM interaction via task()
 * - Cooperative execution via engine yields
 * - BACKTRACKABLE MEMORY via state threading (no external dynamic predicates)
 * - State = state{memory: [...], params: {...}}
 */

:- module(deepclause_mi, [
    parse_dml/5,
    create_engine/6,
    step_engine/4,
    destroy_engine/1,
    % Frame predicates (M0: reentrant, frame-aware execution)
    create_frame/6,
    create_frame_raw/5,
    step_frame/4,
    end_frame/1,
    frame_info/4,
    default_frame/2,
    % REPL session and query mode (M1)
    create_session/4,
    clear_session/1,
    destroy_session/1,
    load_code/3,
    repl_initial_state/3,
    query_start/5,
    repl_solve/4,
    collect_bindings/2,
    repl_listing/2,
    repl_listing/3,
    repl_reserved/1,
    % Freeze lock and export (M2)
    repl_freeze/1,
    repl_unfreeze/1,
    repl_lock/1,
    repl_is_frozen/1,
    repl_try_unfreeze/2,
    export_harness/3,
    export_stats/4,
    repl_script_files/2,
    repl_materialize_embedded/1,
    % Prompt mode (M3)
    repl_meta_prompt/2,
    repl_meta_prompt/3,
    repl_extract_code/2,
    repl_assert/3,
    repl_assert_many/3,
    repl_retract/3,
    repl_define/4,
    repl_delete_predicate/4,
    repl_edit_touched/2,
    repl_edit_touched_text/2,
    repl_edit_touched_clear/1,
    repl_validate_tools/2,
    repl_validate_docs/2,
    schema_fill_fields/3,
    schema_fields_of/3,
    repl_unhandled_directives/2,
    repl_inject_workspace_predicates/1,
    workspace_predicate_name/1,
    repl_set_dml_reference/2,
    repl_set_authoring_examples/2,
    repl_manifest/2,
    repl_history_load/2,
    repl_history_record/2,
    repl_history_summary/2,
    repl_rollback/3,
    repl_user_predicates/2,
    repl_user_predicates_text/2,
    repl_predicate_arities/3,
    repl_probe_nondeterminism/6,
    repl_set_meta_catalog/2,
    repl_set_meta_template/2,
    repl_set_harness_examples/2,
    repl_set_prolog_snippets/2,
    repl_run_meta/4,
    repl_snapshot_save/1,
    repl_snapshot_restore/1,
    repl_commit/3,
    repl_last_commit_summary/2,
    repl_execute_frame/4,
    repl_record_meta/3,
    repl_memory_term/3,
    repl_get_memory/3,
    repl_set_memory/3,
    post_agent_result/4,
    post_exec_result/3,
    post_sample_token_result/3,
    post_llm_result/3,
    post_judgment_result/3,
    provide_input/2,
    provide_input_cancelled/1,
    mi/3,
    % Tool engine predicates for isolated tool execution
    create_tool_engine/4,
    step_tool_engine/4,
    destroy_tool_engine/1,
    post_tool_agent_result/5
]).

:- use_module(deepclause_strings).

%% Allow mi_call/3 clauses to be non-contiguous
:- discontiguous mi_call/3.
:- discontiguous transform_task_calls/3.
:- discontiguous var_to_name/3.

%% Dynamic predicates for session state (engine management only, not memory!)
:- dynamic session_engine/2.       % session_engine(SessionId, Engine) - legacy default engine handle
:- dynamic session_frame/4.        % session_frame(FrameId, SessionId, Kind, Depth)
:- dynamic session_frame_engine/2. % session_frame_engine(FrameId, Engine)
:- dynamic session_default_frame/2. % session_default_frame(SessionId, FrameId)
:- dynamic session_repl_memory/2.   % session_repl_memory(SessionId, HarnessMemory) - object/entry memory
:- dynamic session_meta_memory/2.   % session_meta_memory(SessionId, MetaMemory) - code-writing task memory
:- dynamic session_repl_mode/2.     % session_repl_mode(SessionId, true/false) - gates format/writeln output shims
:- dynamic session_unhandled_directive/2. % session_unhandled_directive(SessionId, Directive)
:- dynamic session_frozen/2.        % session_frozen(SessionId, true/false) - read-only lock
:- dynamic session_locked/2.        % session_locked(SessionId, true/false) - irreversibly frozen (--locked)
:- dynamic session_directive/2.     % session_directive(SessionId, Directive) - recorded :- directives for export
:- dynamic session_meta_catalog/2.  % session_meta_catalog(SessionId, CatalogText) - host tool catalog for the meta prompt
:- dynamic session_dml_reference/2. % session_dml_reference(SessionId, Text) - bundled DML reference for the meta prompt
:- dynamic session_authoring_examples/2. % session_authoring_examples(SessionId, Text) - curated authoring patterns
:- dynamic session_meta_template/2.  % session_meta_template(SessionId, Template) - bundled META_PROMPT.md template
:- dynamic session_harness_examples/2. % session_harness_examples(SessionId, Text) - bundled HARNESS_EXAMPLES.md
:- dynamic session_prolog_snippets/2. % session_prolog_snippets(SessionId, Text) - bundled PROLOG_SNIPPETS.md
:- dynamic session_last_commit/2.    % session_last_commit(SessionId, [Name/Arity]) - predicates the last commit replaced
:- dynamic session_edit_touched/2.   % session_edit_touched(SessionId, Name/Arity) - agent-mode edit transactions
:- dynamic repl_snapshot/2.         % repl_snapshot(SessionId, Snapshot) - transactional edit snapshot
:- dynamic session_params/2.       % session_params(SessionId, ParamsDict)
:- dynamic session_user_tools/2.   % session_user_tools(SessionId, ToolName)
:- dynamic session_user_tool_schema/3. % session_user_tool_schema(SessionId, ToolName, Schema)
:- dynamic session_pending_input/2. % session_pending_input(SessionId, Input)
:- dynamic session_agent_result/2. % session_agent_result(SessionId, Result)
:- dynamic session_exec_result/2.  % session_exec_result(SessionId, Result)
:- dynamic session_sample_token_result/2. % session_sample_token_result(SessionId, Result)
:- dynamic session_llm_result/2. % session_llm_result(SessionId, Result)
:- dynamic session_judgment_result/2. % session_judgment_result(SessionId, Result)
:- dynamic session_trace_log/2.    % session_trace_log(SessionId, TraceLog) - accumulated trace entries
:- dynamic session_trace_enabled/2. % session_trace_enabled(SessionId, true/false)
:- dynamic session_pending_signal/2. % session_pending_signal(SessionId, Signal) - fallback for engine_fetch
:- dynamic session_proc_result/3.  % session_proc_result(SessionId, Key, Value) - process primitive results
:- dynamic session_user_input/2.   % session_user_input(SessionId, Input) - for ask_user tool
:- dynamic session_input_cancelled/1. % session_input_cancelled(SessionId) - input/2 should fail (non-interactive)

%% ============================================================
%% DML Parsing
%% ============================================================

%% parse_dml(+FilePath, +SessionId, +MemoryId, +Params, -Error)
%% Parse a DML file and load its clauses into the session module
%% Also asserts param/2 facts from the Params dict
parse_dml(FilePath, SessionId, _MemoryId, Params, Error) :-
    catch(
        (
            % FilePath is an absolute host temp path, not workspace-relative.
            system:read_file_to_string(FilePath, Code, []),
            assert_params(SessionId, Params),
            parse_dml_string(Code, SessionId),
            Error = none
        ),
        ParseError,
        format(atom(Error), '~w', [ParseError])
    ).

%% assert_params(+SessionId, +Params)
%% Assert param/2 facts from the Params dict into the session module
assert_params(SessionId, Params) :-
    (   is_dict(Params)
    ->  dict_pairs(Params, _, Pairs),
        forall(
            member(Key-Value, Pairs),
            assertz(SessionId:param(Key, Value))
        )
    ;   true
    ).

%% parse_dml_string(+Code, +SessionId)
%% Parse DML code from a string
parse_dml_string(Code, SessionId) :-
    open_string(Code, Stream),
    parse_clauses(Stream, SessionId),
    close(Stream).

%% parse_clauses(+Stream, +SessionId)
%% Read and process all clauses from a stream
%% Uses variable_names to enable compile-time string interpolation
parse_clauses(Stream, SessionId) :-
    read_term(Stream, Term, [module(SessionId), variable_names(Bindings)]),
    (   Term == end_of_file
    ->  true
    ;   preprocess_loaded_term(Term, Bindings, PreparedTerms),
        forall(member(PreparedTerm, PreparedTerms), process_clause(PreparedTerm, SessionId)),
        parse_clauses(Stream, SessionId)
    ).

%% preprocess_loaded_term(+Term, +Bindings, -PreparedTerms)
%% Apply DML source preprocessing before asserting loaded clauses.
%% This keeps task()/prompt() rewriting and string interpolation working,
%% while also running expand_term/2 so DCG rules (`-->`) become executable
%% difference-list predicates before they are asserted into the session module.
preprocess_loaded_term(Term, Bindings, PreparedTerms) :-
    transform_task_calls(Term, Bindings, TransformedTerm),
    expand_interpolations(TransformedTerm, Bindings, InterpolatedTerm),
    expand_loaded_term(InterpolatedTerm, PreparedTerms).

%% expand_loaded_term(+Term, -ExpandedTerms)
%% Runs SWI term expansion and normalizes the result to a list of terms.
expand_loaded_term(Term, ExpandedTerms) :-
    expand_term(Term, Expanded),
    normalize_expanded_terms(Expanded, ExpandedTerms).

%% normalize_expanded_terms(+Expanded, -Terms)
%% expand_term/2 may return a single term or a list. DML does not need the
%% generated non_terminal/1 directives, so skip them and keep the executable
%% clauses only.
normalize_expanded_terms(Expanded, Terms) :-
    (   is_list(Expanded)
    ->  RawTerms = Expanded
    ;   RawTerms = [Expanded]
    ),
    exclude(skip_expanded_term, RawTerms, Terms).

skip_expanded_term((:- non_terminal(_))).

%% ============================================================
%% Task Call Transformation (compile-time)
%% ============================================================

%% transform_task_calls(+Term, +Bindings, -TransformedTerm)
%% Transform task(Desc, Var1, ...) to task_named(Desc, [Var1, ...], ['Name1', ...])
transform_task_calls(Var, _, Var) :- var(Var), !.
transform_task_calls((Head :- Body), Bindings, (Head :- TransformedBody)) :- !,
    transform_task_calls(Body, Bindings, TransformedBody).
transform_task_calls((A, B), Bindings, (TA, TB)) :- !,
    transform_task_calls(A, Bindings, TA),
    transform_task_calls(B, Bindings, TB).
transform_task_calls((A ; B), Bindings, (TA ; TB)) :- !,
    transform_task_calls(A, Bindings, TA),
    transform_task_calls(B, Bindings, TB).
transform_task_calls((A -> B), Bindings, (TA -> TB)) :- !,
    transform_task_calls(A, Bindings, TA),
    transform_task_calls(B, Bindings, TB).
transform_task_calls(\+ A, Bindings, \+ TA) :- !,
    transform_task_calls(A, Bindings, TA).

%% Transform task/1 to task_named/3 with a hidden Summary variable
%% This ensures task/1 always produces a summary in the conversation history
transform_task_calls(task(Desc), _Bindings, task_named(Desc, [_Summary], [typed_var{name: 'Summary', type: string}])) :- !.

%% Transform task/2 to task_named/3
transform_task_calls(task(Desc, V1), Bindings, task_named(Desc, [Var1], [Info1])) :- !,
    extract_var_info(Bindings, V1, Var1, Info1).

%% Transform task/3 to task_named/3
transform_task_calls(task(Desc, V1, V2), Bindings, task_named(Desc, [Var1, Var2], [Info1, Info2])) :- !,
    extract_var_info(Bindings, V1, Var1, Info1),
    extract_var_info(Bindings, V2, Var2, Info2).

%% Transform task/4 to task_named/3
transform_task_calls(task(Desc, V1, V2, V3), Bindings, task_named(Desc, [Var1, Var2, Var3], [Info1, Info2, Info3])) :- !,
    extract_var_info(Bindings, V1, Var1, Info1),
    extract_var_info(Bindings, V2, Var2, Info2),
    extract_var_info(Bindings, V3, Var3, Info3).

%% Transform task/5 to task_named/3
transform_task_calls(task(Desc, V1, V2, V3, V4), Bindings, task_named(Desc, [Var1, Var2, Var3, Var4], [Info1, Info2, Info3, Info4])) :- !,
    extract_var_info(Bindings, V1, Var1, Info1),
    extract_var_info(Bindings, V2, Var2, Info2),
    extract_var_info(Bindings, V3, Var3, Info3),
    extract_var_info(Bindings, V4, Var4, Info4).

%% Transform prompt/2 to prompt_named/3
transform_task_calls(prompt(Desc, V1), Bindings, prompt_named(Desc, [Var1], [Info1])) :- !,
    extract_var_info(Bindings, V1, Var1, Info1).

%% Transform prompt/3 to prompt_named/3
transform_task_calls(prompt(Desc, V1, V2), Bindings, prompt_named(Desc, [Var1, Var2], [Info1, Info2])) :- !,
    extract_var_info(Bindings, V1, Var1, Info1),
    extract_var_info(Bindings, V2, Var2, Info2).

%% Transform prompt/4 to prompt_named/3
transform_task_calls(prompt(Desc, V1, V2, V3), Bindings, prompt_named(Desc, [Var1, Var2, Var3], [Info1, Info2, Info3])) :- !,
    extract_var_info(Bindings, V1, Var1, Info1),
    extract_var_info(Bindings, V2, Var2, Info2),
    extract_var_info(Bindings, V3, Var3, Info3).

%% Recurse into arbitrary compound terms (like with_tools/2, without_tools/2, etc.)
transform_task_calls(Term, Bindings, TransformedTerm) :-
    compound(Term),
    \+ is_list(Term),  % Don't decompose lists
    Term =.. [Functor|Args],
    Args \= [],  % Has at least one argument
    !,
    maplist(transform_task_calls_arg(Bindings), Args, TransformedArgs),
    TransformedTerm =.. [Functor|TransformedArgs].

%% Helper to transform each argument
transform_task_calls_arg(Bindings, Arg, TransformedArg) :-
    transform_task_calls(Arg, Bindings, TransformedArg).

%% Default: atoms and numbers pass through unchanged
transform_task_calls(Term, _, Term).

%% extract_var_info(+Bindings, +Term, -BareVar, -Info)
%% Extracts name and type from a variable term (which might be wrapped)
%% Supported wrappers: string(V), integer(V), boolean(V), float(V), number(V), list(V), object(V)
extract_var_info(Bindings, Term, BareVar, Info) :-
    (   var(Term)
    ->  % Bare variable - default to string
        BareVar = Term,
        var_to_name(Bindings, Term, Name),
        Info = typed_var{name: Name, type: string}
    ;   compound(Term), Term =.. [schema, SchemaName, Inner], var(Inner)
    ->  % A declared shape: schema(name, Var)
        BareVar = Inner,
        var_to_name(Bindings, Inner, Name),
        Info = typed_var{name: Name, type: object, schema: SchemaName}
    ;   compound(Term), Term =.. [Wrapper, Inner]
    ->  % Wrapped variable
        (   wrapper_type(Wrapper, Type)
        ->  (   var(Inner)
            ->  BareVar = Inner,
                var_to_name(Bindings, Inner, Name),
                Info = typed_var{name: Name, type: Type}
            ;   compound(Inner), Inner =.. [schema, SchemaName, InnerVar], var(InnerVar), Wrapper == list
            ->  % List of declared shapes: list(schema(name, X))
                BareVar = InnerVar,
                var_to_name(Bindings, InnerVar, Name),
                Info = typed_var{name: Name, type: array, itemType: object, schema: SchemaName}
            ;   compound(Inner), Inner =.. [InnerWrapper, InnerVar], var(InnerVar), Wrapper == list
            ->  % List of typed items: list(string(X))
                BareVar = InnerVar,
                var_to_name(Bindings, InnerVar, Name),
                wrapper_type(InnerWrapper, ItemType),
                Info = typed_var{name: Name, type: array, itemType: ItemType}
            ;   % Unknown inner structure
                BareVar = Term,
                Info = typed_var{name: "Unknown", type: string}
            )
        ;   % Unknown wrapper
            BareVar = Term,
            Info = typed_var{name: "Unknown", type: string}
        )
    ;   % Not a variable or known wrapper
        BareVar = Term,
        Info = typed_var{name: "Unknown", type: string}
    ).

%% wrapper_type(?Wrapper, ?Type)
wrapper_type(media, media).
wrapper_type(string, string).
wrapper_type(integer, integer).
wrapper_type(float, number).
wrapper_type(number, number).
wrapper_type(boolean, boolean).
wrapper_type(list, array).
wrapper_type(object, object).
wrapper_type(dict, object).

%% ============================================================
%% Source-facing view of user clauses
%%
%% At load time task/N and prompt/N are rewritten to task_named/3 and
%% prompt_named/3, and interpolated descriptions are hoisted into a preceding
%% format/3. Those internal forms must not leak into listing/export — the meta
%% task only knows task/N — so reverse them for display only.
%% ============================================================

%% clean_clause_body(+Body, -CleanBody)
clean_clause_body(Body, Clean) :-
    conj_list(Body, Goals),
    clean_goal_list(Goals, CleanGoals),
    (   CleanGoals = [Single]
    ->  Clean = Single
    ;   list_to_conjunction(CleanGoals, Clean)
    ).

conj_list((A, B), List) :-
    !,
    conj_list(A, LA),
    conj_list(B, LB),
    append(LA, LB, List).
conj_list(Goal, [Goal]).

clean_goal_list([], []) :- !.
clean_goal_list([Goal], [Clean]) :-
    !,
    clean_atomic(Goal, Clean).
clean_goal_list([First, Second | Rest], [CleanFirst | CleanRest]) :-
    (   merge_interpolation(First, Second, CleanFirst)
    ->  clean_goal_list(Rest, CleanRest)
    ;   clean_atomic(First, CleanFirst),
        clean_goal_list([Second | Rest], CleanRest)
    ).

%% merge_interpolation(+FormatGoal, +TaskGoal, -Merged)
%% Fold `format(string(D), T, Args), task_named(D, ...)` back into
%% `task("…{Var}…", ...)`.
merge_interpolation(FormatGoal, TaskGoal, Merged) :-
    FormatGoal = format(string(D), Template, Args),
    (   TaskGoal = task_named(D, Vars, Infos)
    ;   TaskGoal = prompt_named(D, Vars, Infos)
    ),
    task_functor(TaskGoal, Functor),
    interpolate_template(Template, Args, Description),
    clean_task(Functor, Description, Vars, Infos, Merged).

task_functor(task_named(_, _, _), task).
task_functor(prompt_named(_, _, _), prompt).

clean_atomic(task_named(D, Vars, Infos), Clean) :-
    !,
    clean_task(task, D, Vars, Infos, Clean).
clean_atomic(prompt_named(D, Vars, Infos), Clean) :-
    !,
    clean_task(prompt, D, Vars, Infos, Clean).
clean_atomic((A ; B), (CA ; CB)) :-
    !,
    clean_clause_body(A, CA),
    clean_clause_body(B, CB).
clean_atomic((A -> B), (CA -> CB)) :-
    !,
    clean_clause_body(A, CA),
    clean_clause_body(B, CB).
clean_atomic((A -> B ; C), (CA -> CB ; CC)) :-
    !,
    clean_clause_body(A, CA),
    clean_clause_body(B, CB),
    clean_clause_body(C, CC).
clean_atomic(\+(A), \+ CA) :-
    !,
    clean_clause_body(A, CA).
clean_atomic(not(A), not(CA)) :-
    !,
    clean_clause_body(A, CA).
% Wrappers: recurse into their goal arguments so a task/prompt inside is also
% reversed (the load-time rewrite descends into these via transform_task_calls).
clean_atomic(with_tools(Tools, Goal), with_tools(Tools, CleanGoal)) :-
    !,
    clean_clause_body(Goal, CleanGoal).
clean_atomic(without_tools(Tools, Goal), without_tools(Tools, CleanGoal)) :-
    !,
    clean_clause_body(Goal, CleanGoal).
clean_atomic(with_fresh_memory(Goal), with_fresh_memory(CleanGoal)) :-
    !,
    clean_clause_body(Goal, CleanGoal).
clean_atomic(with_judgment(Backend, Goal), with_judgment(Backend, CleanGoal)) :-
    !,
    clean_clause_body(Goal, CleanGoal).
clean_atomic(require_judgment(Backend, Goal), require_judgment(Backend, CleanGoal)) :-
    !,
    clean_clause_body(Goal, CleanGoal).
clean_atomic(once(Goal), once(CleanGoal)) :-
    !,
    clean_clause_body(Goal, CleanGoal).
clean_atomic(ignore(Goal), ignore(CleanGoal)) :-
    !,
    clean_clause_body(Goal, CleanGoal).
clean_atomic(catch(Goal, Catcher, Recovery), catch(CleanGoal, Catcher, CleanRecovery)) :-
    !,
    clean_clause_body(Goal, CleanGoal),
    clean_clause_body(Recovery, CleanRecovery).
clean_atomic(forall(Generator, Action), forall(CleanGenerator, CleanAction)) :-
    !,
    clean_clause_body(Generator, CleanGenerator),
    clean_clause_body(Action, CleanAction).
clean_atomic(findall(Template, Goal, List), findall(Template, CleanGoal, List)) :-
    !,
    clean_clause_body(Goal, CleanGoal).
clean_atomic(Module:Goal, Module:CleanGoal) :-
    !,
    clean_clause_body(Goal, CleanGoal).
clean_atomic(Goal, Goal).

%% clean_task(+Functor, +Description, +Vars, +Infos, -Clean)
clean_task(Functor, Description, Vars, Infos, Clean) :-
    clean_typed_args(Vars, Infos, CleanVars),
    Clean =.. [Functor, Description | CleanVars].

clean_typed_args([], [], []) :- !.
clean_typed_args([Var | Vars], [Info | Infos], [Wrapped | Wrappeds]) :-
    !,
    typed_arg(Var, Info, Wrapped),
    clean_typed_args(Vars, Infos, Wrappeds).
clean_typed_args(Vars, _, Vars).

%% typed_arg(+Var, +Info, -Wrapped) — reverse typed_var{...} to its wrapper.
typed_arg(Var, Info, Wrapped) :-
    is_dict(Info),
    get_dict(type, Info, Type),
    !,
    typed_arg_type(Type, Var, Info, Wrapped).
typed_arg(Var, _, Var).

typed_arg_type(media, Var, _, media(Var)) :- !.
typed_arg_type(string, Var, _, Var) :- !.
typed_arg_type(integer, Var, _, integer(Var)) :- !.
typed_arg_type(number, Var, _, number(Var)) :- !.
typed_arg_type(boolean, Var, _, boolean(Var)) :- !.
typed_arg_type(object, Var, _, object(Var)) :- !.
typed_arg_type(array, Var, Info, list(Wrapped)) :-
    !,
    (   get_dict(itemType, Info, ItemType)
    ->  true
    ;   ItemType = string
    ),
    Wrapped =.. [ItemType, Var].
typed_arg_type(_, Var, _, Var).

%% interpolate_template(+Template, +Args, -Description)
%% Reverse of the {Var} -> ~w hoist. Succeeds only when every placeholder is a
%% plain variable, so the caller can fall back to keeping the format/3 goal.
interpolate_template(Template, Args, Description) :-
    string(Template),
    string_codes(Template, Codes),
    interpolate_codes(Codes, Args, OutCodes),
    string_codes(Description, OutCodes).

interpolate_codes([], [], []) :- !.
%% A {Var} placeholder consumes one argument; require Args to be exhausted
%% exactly (the []/[] base case above fails if either side has leftovers).
interpolate_codes([0'~, 0'w | Rest], [Arg | Args], Out) :-
    !,
    named_var(Arg),
    term_string(Arg, Name, [numbervars(true)]),
    string_codes(Name, NameCodes),
    interpolate_codes(Rest, Args, Tail),
    append([0'{|NameCodes], [0'}|Tail], Out).
interpolate_codes([0'~, 0'~ | Rest], Args, [0'~ | Out]) :-
    !,
    interpolate_codes(Rest, Args, Out).
interpolate_codes([Code | Rest], Args, [Code | Out]) :-
    interpolate_codes(Rest, Args, Out).

%% An interpolation argument is a variable or a `$VAR(N)` term (after
%% numbervars), both of which print as a stable name (A, B, ...).
named_var(Arg) :- var(Arg), !.
named_var('$VAR'(_)).

%% var_to_name(+Bindings, +Var, -Name)
%% Look up a variable's name from the bindings list
var_to_name(Bindings, Var, Name) :-
    (   member(N=V, Bindings), V == Var
    ->  Name = N
    ;   Name = 'Var'  % Fallback if not found
    ).

%% process_clause(+Term, +SessionId)
%% Process a single clause and add to session.
%% Reject reserved-name definitions (third place of the reserved guard, so
%% consulted/generated code cannot shadow the REPL control surface).
process_clause(Term, _SessionId) :-
    repl_reserved_clause(Term),
    !,
    throw(error(permission_error(modify, reserved_name, Term),
                context(process_clause/2, 'reserved predicate'))).

process_clause((:- Directive), SessionId) :-
    !,
    process_directive(Directive, SessionId).

%% Handle tool/2 with description: tool(Head, Description) :- Body
process_clause((tool(ToolHead, Description) :- Body), SessionId) :-
    !,
    (catch(expand_dict_dots(Body, ExpandedBody), _, fail) -> true ; ExpandedBody = Body),
    extract_tool_schema(ToolHead, Description, ToolName, Schema),
    assertz(session_user_tools(SessionId, ToolName)),
    assertz(session_user_tool_schema(SessionId, ToolName, Schema)),
    % Store the source code for the tool description. Reverses the load-time
    % task/prompt rewrites and normalizes variables for deterministic export.
    format_tool_source(ToolHead, Description, Body, SourceCode),
    assertz(SessionId:tool_source(ToolName, SourceCode)),
    % Assert the tool implementation (use just ToolHead for execution)
    assertz(SessionId:(tool(ToolHead) :- ExpandedBody)).

%% Handle tool/1 without description: tool(Head) :- Body
process_clause((tool(ToolHead) :- Body), SessionId) :-
    !,
    (catch(expand_dict_dots(Body, ExpandedBody), _, fail) -> true ; ExpandedBody = Body),
    extract_tool_schema(ToolHead, none, ToolName, Schema),
    assertz(session_user_tools(SessionId, ToolName)),
    assertz(session_user_tool_schema(SessionId, ToolName, Schema)),
    % Store the source code for the tool description (reverses task rewrites).
    format_tool_source(ToolHead, none, Body, SourceCode),
    assertz(SessionId:tool_source(ToolName, SourceCode)),
    % Assert the tool implementation
    assertz(SessionId:(tool(ToolHead) :- ExpandedBody)).

process_clause((Head :- Body), SessionId) :-
    !,
    % Expand dict dot-notation (Dict.Key → get_dict), then assert
    (   catch(expand_dict_dots(Body, ExpandedBody), _, fail)
    ->  true
    ;   ExpandedBody = Body
    ),
    assertz(SessionId:(Head :- ExpandedBody)).

%% repl_delete/1 is a load-time directive that removes a predicate. It must
%% be handled before the generic fact clause so it is never asserted itself.
process_clause(repl_delete(Name/Arity), SessionId) :-
    !,
    (   atom(Name), integer(Arity)
    ->  catch(abolish(SessionId:Name/Arity), _, true)
    ;   true
    ).

process_clause(Fact, SessionId) :-
    % Simple fact
    assertz(SessionId:Fact).

%% ============================================================
%% Dict Dot-Notation Expansion (compile-time)
%% ============================================================
%% SWI-Prolog's dict functional notation (Dict.Key) is normally
%% expanded by goal_expansion during compilation. Since DML clauses
%% are loaded via assertz (no goal expansion), we must expand
%% Dict.Key → get_dict(Key, Dict, Value) manually before asserting.
%%
%% Handles:
%%   Dict.Key = Value   →  get_dict(Key, Dict, Value)
%%   Value = Dict.Key   →  get_dict(Key, Dict, Value)
%%   Dict.Key == Value  →  get_dict(Key, Dict, Tmp), Tmp == Value
%%   Value == Dict.Key  →  get_dict(Key, Dict, Tmp), Tmp == Value

%% expand_dict_dots(+GoalIn, -GoalOut)
%% Top-level expansion for goal bodies
expand_dict_dots(Var, Var) :- var(Var), !.
expand_dict_dots((A, B), (EA, EB)) :- !, expand_dict_dots(A, EA), expand_dict_dots(B, EB).
expand_dict_dots((A ; B), (EA ; EB)) :- !, expand_dict_dots(A, EA), expand_dict_dots(B, EB).
expand_dict_dots((A -> B), (EA -> EB)) :- !, expand_dict_dots(A, EA), expand_dict_dots(B, EB).

%% Unification with dict dot access: Dict.Key = Value
expand_dict_dots(Goal, Result) :-
    nonvar(Goal),
    functor(Goal, =, 2),
    !,
    arg(1, Goal, A),
    arg(2, Goal, B),
    (   nonvar(A), is_dot_access(A, Dict, Key)
    ->  Result = get_dict(Key, Dict, B)
    ;   nonvar(B), is_dot_access(B, Dict, Key)
    ->  Result = get_dict(Key, Dict, A)
    ;   Result = Goal
    ).

%% Equality check with dict dot access: Dict.Key == Value
expand_dict_dots(Goal, Result) :-
    nonvar(Goal),
    functor(Goal, ==, 2),
    !,
    arg(1, Goal, A),
    arg(2, Goal, B),
    (   nonvar(A), is_dot_access(A, Dict, Key)
    ->  Result = (get_dict(Key, Dict, Tmp), Tmp == B)
    ;   nonvar(B), is_dot_access(B, Dict, Key)
    ->  Result = (get_dict(Key, Dict, Tmp), Tmp == A)
    ;   Result = Goal
    ).

expand_dict_dots(Goal, Goal).

%% is_dot_access(+Term, -Dict, -Key)
%% Check if Term is a dict dot-access expression: '.'(Dict, Key) where Key is atom
is_dot_access(Term, Dict, Key) :-
    compound(Term),
    compound_name_arity(Term, '.', 2),
    arg(1, Term, Dict),
    arg(2, Term, Key),
    atom(Key).

%% expand_dict_expr(+ExprIn, -ExprOut)
%% Expand dict access in value-level expressions (just pass through for now)
expand_dict_expr(Var, Var) :- var(Var), !.
expand_dict_expr(Expr, Expr).

%% ============================================================
%% Tool Schema Extraction
%% ============================================================

%% extract_tool_schema(+ToolHead, +Description, -ToolName, -Schema)
extract_tool_schema(ToolHead, Description, ToolName, Schema) :-
    ToolHead =.. [ToolName|Args],
    length(Args, Arity),
    extract_params(Args, 1, Arity, Inputs, Outputs),
    (Description == none -> Desc = "" ; Desc = Description),
    Schema = schema{
        name: ToolName,
        description: Desc,
        inputs: Inputs,
        outputs: Outputs
    }.

%% extract_params(+Args, +Index, +Arity, -Inputs, -Outputs)
extract_params([], _, _, [], []) :- !.
extract_params([_Arg|Rest], Index, Arity, Inputs, Outputs) :-
    format(atom(Name), 'arg~w', [Index]),
    Type = string,
    NextIndex is Index + 1,
    extract_params(Rest, NextIndex, Arity, RestInputs, RestOutputs),
    Param = param{name: Name, type: Type},
    (   Index == Arity
    ->  Inputs = RestInputs, Outputs = [Param|RestOutputs]
    ;   Inputs = [Param|RestInputs], Outputs = RestOutputs
    ).

module_reference_string(Module, ModuleStr) :-
    atom(Module),
    !,
    atom_string(Module, ModuleStr).
module_reference_string(Module, Module) :-
    string(Module).

workspace_module_reference(Module) :-
    module_reference_string(Module, ModuleStr),
    (   sub_string(ModuleStr, _, _, _, "/")
    ;   sub_string(ModuleStr, _, _, 0, ".dml")
    ;   sub_string(ModuleStr, _, _, 0, ".pl")
    ;   sub_string(ModuleStr, 0, 1, _, ".")
    ).

process_use_module_directive(Module, SessionId) :-
    (   workspace_module_reference(Module)
    ->  process_directive(consult(Module), SessionId)
    ;   SessionId:use_module(Module),
        import_library_into_mi(Module)
    ).

process_use_module_directive(Module, Exports, SessionId) :-
    (   workspace_module_reference(Module)
    ->  process_directive(consult(Module), SessionId)
    ;   SessionId:use_module(Module, Exports),
        import_library_into_mi(Module)
    ).

%% System libraries are also imported into the meta-interpreter module itself,
%% so meta-calls (`call_with_inference_limit/3`, `once/1`, `catch/3`, ...) can
%% resolve them. Without this, `call_with_inference_limit(label(Xs), ...)` fails
%% with `existence_error(deepclause_mi:label/1)` because clpfd was imported only
%% into the session module.
import_library_into_mi(library(Name)) :-
    !,
    catch(deepclause_mi:use_module(library(Name)), _, true).
import_library_into_mi(_).

%% process_directive(+Directive, +SessionId)
process_directive(param(Key, Desc), SessionId) :-
    !,
    assertz(SessionId:param_decl(Key, Desc)).
process_directive(param(Key, Desc, Default), SessionId) :-
    !,
    assertz(SessionId:param_decl(Key, Desc, Default)).

%% Handle consult directive: :- consult(File)
process_directive(consult(File), SessionId) :-
    !,
    % Set session ID for get_session_id to find, then call our custom consult
    nb_setval(current_session_id, SessionId),
    system:read_file_to_string(File, Code, []),
    consult_string(Code, SessionId).
%% Handle use_module directive
process_directive(use_module(Module), SessionId) :-
    !,
    (   workspace_module_reference(Module)
    ->  true
    ;   record_directive(SessionId, use_module(Module))
    ),
    process_use_module_directive(Module, SessionId).
process_directive(use_module(Module, Exports), SessionId) :-
    !,
    (   workspace_module_reference(Module)
    ->  true
    ;   record_directive(SessionId, use_module(Module, Exports))
    ),
    process_use_module_directive(Module, Exports, SessionId).
%% Handle list-style consult: :- [File] or :- [File1, File2, ...]
process_directive([File|Files], SessionId) :-
    !,
    nb_setval(current_session_id, SessionId),
    process_directive(consult(File), SessionId),
    (Files == [] -> true ; process_directive(Files, SessionId)).
%% `:- repl_delete(Name/Arity).` — the directive form of the fact handled by
%% process_clause/2 below. It used to fall through to the catch-all and be
%% discarded, so a predicate the author asked to remove stayed in the module.
process_directive(repl_delete(Name/Arity), SessionId) :-
    !,
    atom(Name),
    integer(Arity),
    Arity >= 0,
    catch(abolish(SessionId:Name/Arity), _, true).

%% process_directive(require_sandbox, +SessionId)
%% The harness declares that it must run in an execution sandbox, and the load
%% fails if it cannot. Checked against the **effective** backend rather than
%% mere availability: bwrap being installed while the session resolves to
%% clean-room means the code would run unsandboxed, which is what the directive
%% exists to prevent. Only the user sets this (see repl_commit_directive/2).
process_directive(require_sandbox, SessionId) :-
    (   session_sandbox_backend(SessionId, Kind)
    ->  true
    ;   Kind = 'clean-room'
    ),
    (   sandbox_backend_acceptable(Kind)
    ->  true
    ;   format(atom(Message),
            'harness requires an execution sandbox, but the effective backend is ~w; bwrap, AgentVM or sandbox-exec is required. Install bubblewrap, run with --sandbox (AgentVM), or remove the :- require_sandbox. directive.',
            [Kind]),
        throw(error(permission_error(load, harness, require_sandbox),
                    context(require_sandbox/0, Message)))
    ).

%% Anything else is recorded rather than discarded. An unrecognised directive
%% (`:- dynamic(f/1).`, a `:- op(...)` without use_module, a typo) does nothing,
%% and silently doing nothing is how an edit that never took effect goes
%% unnoticed. The host reports these after a load.
process_directive(Directive, SessionId) :-
    (   repl_benign_directive(Directive)
    ->  true
    ;   assertz(session_unhandled_directive(SessionId, Directive))
    ).

%% Directives DML deliberately accepts and ignores (SWI state it manages itself).
repl_benign_directive(dynamic(_)).
repl_benign_directive(dynamic(_, _)).
repl_benign_directive(discontiguous(_)).
repl_benign_directive(discontiguous(_, _)).
repl_benign_directive(multifile(_)).
repl_benign_directive(multifile(_, _)).

%% sandbox_kind_atom(+Raw, -Atom)
%% A JS string or an atom in, an atom out.
%%
%% Uses term_string/3, not atom_string/2: in this WASM build `atom_string/2`
%% returns the string unchanged, so an "atom" produced by it still fails `atom/1`
%% and compares unequal to every atom literal. That silently made every backend
%% look unacceptable. `term_string(Atom, "bwrap", [])` does produce the atom.
sandbox_kind_atom(Raw, Atom) :-
    (   atom(Raw)
    ->  Atom = Raw
    ;   catch(term_string(Atom, Raw, []), _, Atom = Raw)
    ).

%% sandbox_backend_acceptable(+Kind)
%% The backends that provide filesystem isolation for `exec(bash(...))`.
%% `clean-room` is the absence of a wrapper and does not qualify. The host
%% normalizes `sandbox-exec` to `sandbox_exec` (see DMLSession.open) because a
%% hyphen makes the token a subtraction expression to a Prolog parser.
sandbox_backend_acceptable(agentvm).
sandbox_backend_acceptable(bwrap).
sandbox_backend_acceptable(sandbox_exec).


%% repl_unhandled_directives(+SessionId, -Directives)
repl_unhandled_directives(SessionId, Directives) :-
    findall(D, session_unhandled_directive(SessionId, D), Directives).

%% ============================================================
%% Engine Management
%% ============================================================

%% create_engine(+SessionId, +MemoryId, +Args, +Params, +InitialMemory, -Engine)
%% Args is a list of positional arguments for agent_main
%% Params is a dict of named parameters (already asserted as param/2 facts)
%%
%% The engine is registered as the session's default `query` frame so that
%% step_engine/4 keeps working as a thin wrapper over step_frame/4.
create_engine(SessionId, _MemoryId, Args, Params, InitialMemory, Engine) :-
    assertz(session_params(SessionId, Params)),
    determine_agent_goal(SessionId, Args, Goal),
    % Check if tracing is enabled and store in session state
    (get_dict(trace, Params, true) -> TraceEnabled = true ; TraceEnabled = false),
    assertz(session_trace_enabled(SessionId, TraceEnabled)),
    assertz(session_trace_log(SessionId, [])),
    % Check for gas limit in params
    (get_dict(gas_limit, Params, GasLimit) -> true ; GasLimit = -1),
    % Create initial state with provided memory, params, context stack, trace depth and gas
    InitialState = state{memory: InitialMemory, params: Params, context_stack: [], depth: 0, gas_remaining: GasLimit},
    create_frame(SessionId, query, 0, Goal, InitialState, FrameId),
    retractall(session_default_frame(SessionId, _)),
    assertz(session_default_frame(SessionId, FrameId)),
    session_frame_engine(FrameId, Engine),
    retractall(session_engine(SessionId, _)),
    assertz(session_engine(SessionId, Engine)).

%% ============================================================
%% Frames (re-entrant execution)
%% ============================================================
%%
%% Historically a session owned exactly one engine (session_engine/2).
%% Prompt mode requires nested engines: a meta frame that calls `execute`
%% which runs a child frame, which may itself call task() and spawn a
%% further frame. A frame is therefore (FrameId, SessionId, Kind, Depth)
%% with its engine stored in session_frame_engine/2.
%%
%% The legacy step_engine/4 resolves the session's default query frame and
%% delegates to step_frame/4, so single-engine callers are unaffected.

default_frame(SessionId, FrameId) :-
    session_default_frame(SessionId, FrameId).

%% frame_info(+FrameId, -SessionId, -Kind, -Depth)
frame_info(FrameId, SessionId, Kind, Depth) :-
    session_frame(FrameId, SessionId, Kind, Depth).

%% create_frame_raw(+SessionId, +Kind, +Depth, +EngineGoal, -FrameId)
%% Registers a frame whose engine runs EngineGoal verbatim. Most callers want
%% create_frame/6, which wraps the goal in the mi/3 meta-interpreter; goal mode
%% uses this to run repl_solve/4 directly (mi/3 cuts after the first solution).
create_frame_raw(SessionId, Kind, Depth, EngineGoal, FrameId) :-
    gensym(frame_, FrameId),
    engine_create(_, EngineGoal, Engine),
    assertz(session_frame(FrameId, SessionId, Kind, Depth)),
    assertz(session_frame_engine(FrameId, Engine)).

%% create_frame(+SessionId, +Kind, +Depth, +Goal, +InitialState, -FrameId)
%% Kind = query | meta | nested | object
%% Creates an engine running deepclause_mi:mi(Goal, InitialState, SessionId)
%% and registers it as a frame. The caller is responsible for depth-first
%% stepping: a parent frame is suspended while a child frame runs, so the
%% per-session result/signal slots are never concurrently pending.
create_frame(SessionId, Kind, Depth, Goal, InitialState, FrameId) :-
    create_frame_raw(SessionId, Kind, Depth,
        deepclause_mi:mi(Goal, InitialState, SessionId),
        FrameId).

%% step_frame(+FrameId, -Status, -Content, -Payload)
step_frame(FrameId, Status, Content, Payload) :-
    (   session_frame(FrameId, SessionId, _Kind, _Depth),
        session_frame_engine(FrameId, Engine)
    ->  step_engine_handle(SessionId, Engine, Status, Content, Payload)
    ;   Status = error,
        Content = 'No engine found for frame',
        Payload = none
    ).

%% step_engine_handle(+SessionId, +Engine, -Status, -Content, -Payload)
%% Shared stepping logic for both step_frame/4 and the legacy step_engine/4.
step_engine_handle(SessionId, Engine, Status, Content, Payload) :-
    catch(
        (   engine_next(Engine, Result)
        ->  process_engine_result(Result, Status, Content, Payload)
        ;   % Engine finished - return trace if enabled
            Status = finished,
            Content = '',
            (   session_trace_enabled(SessionId, true),
                session_trace_log(SessionId, TraceLog)
            ->  Payload = payload{trace: TraceLog}
            ;   Payload = none
            )
        ),
        Error,
        (   Error = process_exit(_)
        ->  Status = finished,
            Content = '',
            Payload = none
        ;   format(atom(ErrMsg), 'Engine error: ~w', [Error]),
            Status = error,
            Content = ErrMsg,
            (   session_trace_enabled(SessionId, true),
                session_trace_log(SessionId, TraceLog)
            ->  Payload = payload{trace: TraceLog}
            ;   Payload = none
            )
        )
    ).

%% end_frame(+FrameId)
%% Destroys the frame's engine and unregisters the frame. Also clears the
%% session's default-frame pointer if this frame was the default.
end_frame(FrameId) :-
    (   session_frame_engine(FrameId, Engine)
    ->  catch(engine_destroy(Engine), _, true)
    ;   true
    ),
    (   session_frame(FrameId, SessionId, _, _)
    ->  retractall(session_default_frame(SessionId, FrameId))
    ;   true
    ),
    retractall(session_frame_engine(FrameId, _)),
    retractall(session_frame(FrameId, _, _, _)).

%% ============================================================
%% REPL Session and Goal Mode (M1)
%% ============================================================
%%
%% A REPL session owns a persistent living module. Unlike create_engine/6,
%% which is tied to a single run and destroyed by destroy_engine/1, a session
%% lives across inputs; reset/clear_session empties it without tearing down the
%% process-level SWI instance.

%% create_session(+SessionId, +Params, +HarnessMemory, +MetaMemory)
create_session(SessionId, Params, HarnessMemory, MetaMemory) :-
    retractall(session_params(SessionId, _)),
    assertz(session_params(SessionId, Params)),
    (   get_dict(trace, Params, true)
    ->  TraceEnabled = true
    ;   TraceEnabled = false
    ),
    retractall(session_trace_enabled(SessionId, _)),
    assertz(session_trace_enabled(SessionId, TraceEnabled)),
    retractall(session_trace_log(SessionId, _)),
    assertz(session_trace_log(SessionId, [])),
    retractall(session_repl_memory(SessionId, _)),
    assertz(session_repl_memory(SessionId, HarnessMemory)),
    retractall(session_meta_memory(SessionId, _)),
    assertz(session_meta_memory(SessionId, MetaMemory)),
    retractall(session_repl_mode(SessionId, _)),
    assertz(session_repl_mode(SessionId, true)),
    retractall(session_frozen(SessionId, _)),
    assertz(session_frozen(SessionId, false)),
    retractall(session_locked(SessionId, _)),
    assertz(session_locked(SessionId, false)),
    % Stored as an atom. The host sends a JS string, and an atom and a string
    % that print identically are not `==`, so normalize once here rather than at
    % every comparison.
    (   get_dict(sandbox, Params, SandboxRaw)
    ->  sandbox_kind_atom(SandboxRaw, SandboxKind)
    ;   SandboxKind = 'clean-room'
    ),
    retractall(session_sandbox_backend(SessionId, _)),
    assertz(session_sandbox_backend(SessionId, SandboxKind)),
    repl_inject_workspace_predicates(SessionId).

%% repl_abolish_user_predicates(+SessionId)
%% Removes only the harness's own predicates; imported builtins are untouched.
repl_abolish_user_predicates(SessionId) :-
    (   current_module(SessionId)
    ->  findall(Name/Arity,
            (   current_predicate(SessionId:Name/Arity),
                repl_user_predicate(SessionId, Name, Arity)
            ),
            UserPreds0),
        sort(UserPreds0, UserPreds),
        forall(member(Name/Arity, UserPreds),
            catch(abolish(SessionId:Name/Arity), _, true))
    ;   true
    ).

%% clear_session(+SessionId)
%% Empties the living module, ends all frames, and resets memories. Session
%% params (workspace/gas limit) are preserved so the REPL stays configured.
clear_session(SessionId) :-
    (   findall(FrameId, session_frame(FrameId, SessionId, _, _), FrameIds)
    ->  forall(member(FrameId, FrameIds), end_frame(FrameId))
    ;   true
    ),
    (   session_engine(SessionId, Engine)
    ->  catch(engine_destroy(Engine), _, true)
    ;   true
    ),
    retractall(session_engine(SessionId, _)),
    retractall(session_default_frame(SessionId, _)),
    retractall(session_repl_memory(SessionId, _)),
    assertz(session_repl_memory(SessionId, [])),
    retractall(session_meta_memory(SessionId, _)),
    assertz(session_meta_memory(SessionId, [])),
    retractall(repl_snapshot(SessionId, _)),
    retractall(session_user_tools(SessionId, _)),
    retractall(session_user_tool_schema(SessionId, _, _)),
    repl_abolish_user_predicates(SessionId).

%% destroy_session(+SessionId)
%% Tears a REPL session down completely: clears the module/frames and removes
%% the session's dynamic bookkeeping so the process can host another session.
destroy_session(SessionId) :-
    clear_session(SessionId),
    retractall(session_repl_memory(SessionId, _)),
    retractall(session_meta_memory(SessionId, _)),
    retractall(session_repl_mode(SessionId, _)),
    retractall(session_frozen(SessionId, _)),
    retractall(session_locked(SessionId, _)),
    retractall(session_directive(SessionId, _)),
    retractall(session_meta_catalog(SessionId, _)),
    retractall(session_dml_reference(SessionId, _)),
    retractall(session_authoring_examples(SessionId, _)),
    retractall(session_meta_template(SessionId, _)),
    retractall(session_harness_examples(SessionId, _)),
    retractall(session_prolog_snippets(SessionId, _)),
    retractall(session_last_commit(SessionId, _)),
    retractall(repl_snapshot(SessionId, _)),
    retractall(session_params(SessionId, _)),
    retractall(session_trace_enabled(SessionId, _)),
    retractall(session_trace_log(SessionId, _)).

%% load_code(+SessionId, +Code, -Error)
%% Parses DML source into the session module, running the normal source
%% preprocessing (task/prompt rewrite, interpolation, expand_term).
load_code(SessionId, Code, Error) :-
    catch(
        (   guard_not_frozen(SessionId),
            parse_dml_string(Code, SessionId),
            Error = none
        ),
        LoadError,
        format(atom(Error), '~w', [LoadError])
    ).

%% repl_initial_state(+SessionId, +FrameKind, -InitialState)
%% Builds the state{} threaded through a REPL frame. M1 seeds harness memory
%% from session_repl_memory/2; M4 adds the meta/scratch slots.
repl_initial_state(SessionId, FrameKind, InitialState) :-
    session_params(SessionId, Params),
    (   get_dict(gas_limit, Params, GasLimit)
    ->  true
    ;   GasLimit = -1
    ),
    % Two-level memory: meta (code-writing) and harness (runtime) are kept
    % apart. The state carries the slot so write-back goes to the right one.
    (   FrameKind == meta
    ->  MemorySlot = meta,
        (   session_meta_memory(SessionId, Memory)
        ->  true
        ;   Memory = []
        )
    ;   MemorySlot = harness,
        (   session_repl_memory(SessionId, Memory)
        ->  true
        ;   Memory = []
        )
    ),
    InitialState = state{
        memory: Memory,
        params: Params,
        context_stack: [],
        depth: 0,
        gas_remaining: GasLimit,
        memory_slot: MemorySlot,
        frame_kind: FrameKind
    }.

%% query_start(+SessionId, +Text, +FrameKind, +InitialState, -Error)
%% Parses and preprocesses a goal string, then creates a frame running
%% repl_solve/4. The frame becomes the session's default frame so that
%% step_engine/4 and the runner can drive it.
query_start(SessionId, Text, FrameKind, InitialState, Error) :-
    catch(
        (   read_repl_goal(SessionId, Text, Goal, Names),
            create_frame_raw(SessionId, FrameKind, 0,
                deepclause_mi:repl_solve(Goal, Names, SessionId, InitialState),
                FrameId),
            retractall(session_default_frame(SessionId, _)),
            assertz(session_default_frame(SessionId, FrameId)),
            session_frame_engine(FrameId, Engine),
            retractall(session_engine(SessionId, _)),
            assertz(session_engine(SessionId, Engine)),
            Error = none
        ),
        QueryError,
        format(atom(Error), '~w', [QueryError])
    ).

%% ensure_full_stop(+Text, -TextWithStop)
%% Appends a terminating full stop + newline when the goal text lacks one, so
%% both `goal: p(X).` and `-g "p(X)"` read cleanly.
ensure_full_stop(Text, Text1) :-
    atom_string(TextAtom, Text),
    (   atom_ends_with_stop(TextAtom)
    ->  atom_concat(TextAtom, '\n', Text1)
    ;   atom_concat(TextAtom, '.\n', Text1)
    ).

%% atom_ends_with_stop(+Atom)
%% True when the last non-whitespace character is a full stop. A naive
%% "ends with ." test would append an extra full stop to any block that ends
%% with a newline, and read_term/3 then fails on the lone '.' (end_of_clause).
atom_ends_with_stop(TextAtom) :-
    atom_string(TextAtom, TextStr),
    string_length(TextStr, Len0),
    trim_trailing_ws(TextStr, Len0, Len),
    Len > 0,
    Pos is Len - 1,
    sub_string(TextStr, Pos, 1, _, ".").

trim_trailing_ws(Str, Len0, Len) :-
    (   Len0 > 0,
        Pos is Len0 - 1,
        sub_string(Str, Pos, 1, _, Ch),
        ( Ch == " " ; Ch == "\t" ; Ch == "\n" ; Ch == "\r" )
    ->  Len1 is Len0 - 1,
        trim_trailing_ws(Str, Len1, Len)
    ;   Len = Len0
    ).

%% read_repl_goal(+SessionId, +Text, -Goal, -Names)
%% Names is the read_term variable_names list (Name=Var), used to render
%% bindings for solution events.
read_repl_goal(SessionId, Text, Goal, Names) :-
    % read_term/3 requires a full stop; REPL callers may omit it (`-g "p(X)"`).
    ensure_full_stop(Text, Text1),
    open_string(Text1, Stream),
    read_term(Stream, RawGoal, [module(SessionId), variable_names(Names0)]),
    close(Stream),
    (   RawGoal == end_of_file
    ->  throw(error(syntax_error('empty goal'), _))
    ;   true
    ),
    % Apply only the goal-relevant source transforms. Do NOT run
    % preprocess_loaded_term/3 here: its file-load term expansion flattens a
    % list goal such as ["file.dml"] into its elements.
    transform_task_calls(RawGoal, Names0, Transformed),
    % Use the body expander (not expand_term_interp/3) so a `{Var}` string is
    % hoisted into a goal *before* the goal that uses it. expand_term_interp
    % embeds the interpolation term in the argument position instead, which is
    % wrong for a raw goal such as verify(X, "Is {X} > 0?", V).
    expand_body_interp(Transformed, Names0, Goal),
    Names = Names0.

%% repl_solve(+Goal, +Names, +SessionId, +State)
%% Enumerating goal-mode solver. Yields one solution(Bindings, Deterministic)
%% per answer and fails so engine_next/2 backtracks for the next solution.
%% answer/1 commits (throws '$answer_commit') and stops enumeration.
%% mi/3 is deliberately not used: it cuts after the first solution.
repl_solve(Goal, Names, SessionId, State) :-
    nb_setval(current_session_id, SessionId),
    catch(
        repl_solve_loop(Goal, Names, SessionId, State),
        '$answer_commit',
        true
    ).

repl_solve_loop(Goal, Names, SessionId, State) :-
    mi_call(Goal, State, StateOut),
    collect_bindings(Names, Bindings),
    store_memory_for_state(SessionId, StateOut),
    engine_yield(solution(Bindings, false)),
    fail.

%% collect_bindings(+Names, -Bindings)
%% Bindings is a list of Name-RenderedText. Unbound variables render as '_'.
collect_bindings(Names, Bindings) :-
    findall(Name-Text,
        (   member(Name=Var, Names),
            render_binding(Var, Text)
        ),
        Bindings).

render_binding(Var, '_') :-
    var(Var),
    !.
render_binding(Value, Text) :-
    with_output_to(string(Text), write_term(Value, [quoted(true), max_depth(20)])).

%% store_memory_for_state(+SessionId, +State)
%% Writes the threaded memory back to the slot named by the state.
store_memory_for_state(SessionId, State) :-
    (   is_dict(State),
        get_dict(memory, State, Memory)
    ->  (   get_dict(memory_slot, State, meta)
        ->  retractall(session_meta_memory(SessionId, _)),
            assertz(session_meta_memory(SessionId, Memory))
        ;   retractall(session_repl_memory(SessionId, _)),
            assertz(session_repl_memory(SessionId, Memory))
        )
    ;   true
    ).

%% repl_listing(+SessionId, +Filter, -Text)
%% Read-only listing used by the meta tool and the `listing.` REPL command.
repl_listing(SessionId, Filter, Text) :-
    with_output_to(string(Text), repl_listing_output(SessionId, Filter)).

%% repl_listing(+Filter, -Text) uses the current session id.
repl_listing(Filter, Text) :-
    get_session_id(SessionId),
    repl_listing(SessionId, Filter, Text).

repl_listing_output(SessionId, Filter) :-
    (   Filter == tools
    ->  forall(session_user_tool_schema(SessionId, Name, Schema),
            (   SessionId:tool_source(Name, Source)
            ->  format("~w - ~w~n~w~n~n", [Name, Schema, Source])
            ;   format("~w - ~w~n~n", [Name, Schema])
            )
        )
    ;   Filter == history
    ->  repl_history_listing(SessionId)
    ;   (   current_predicate(SessionId:module_doc/1),
            SessionId:module_doc(Skill)
        ->  format("skill: ~w~n~n", [Skill])
        ;   true
        ),
        findall(Name/Arity,
            (   current_predicate(SessionId:Name/Arity),
                repl_user_predicate(SessionId, Name, Arity)
            ),
            Preds0),
        sort(Preds0, Preds),
        forall(member(Name/Arity, Preds),
            (   repl_listing_match(Filter, Name, Arity)
            ->  format("~w/~w~n", [Name, Arity]),
                (   current_predicate(SessionId:doc/2),
                    SessionId:doc(Name/Arity, DocText)
                ->  format("  % ~w~n", [DocText])
                ;   true
                ),
                functor(Head, Name, Arity),
                forall(clause(SessionId:Head, ClauseBody), print_user_clause(Head, ClauseBody))
            ;   true
            )
        )
    ).

%% A filter either names a predicate (`name/arity`), names a prefix, or is `all`.
%% Anything else matches **nothing**.
%%
%% There used to be a final `repl_listing_match(_, _, _)` catch-all, which turned any
%% filter it did not understand into "the whole module" — silently. `listing('alpha/1')`
%% therefore listed every predicate, so the side view's drill-down showed the module
%% instead of the predicate under the cursor.
repl_listing_match(all, _, _) :- !.
repl_listing_match(Name/Arity, Name, Arity) :- !.
repl_listing_match(Filter, Name, _) :-
    atom(Filter),
    !,
    (   sub_atom(Name, 0, _, _, Filter)
    ;   sub_atom(Filter, 0, _, _, Name)
    ).
repl_listing_match(_, _, _) :-
    fail.

%% repl_user_predicate(+SessionId, +Name, +Arity)
%% Excludes reserved and internal predicates from listing/export.
repl_user_predicate(_SessionId, Name, _Arity) :-
    repl_reserved_name(Name),
    !,
    fail.
repl_user_predicate(_SessionId, Name, _Arity) :-
    internal_session_predicate(Name),
    !,
    fail.
repl_user_predicate(_SessionId, Name, _Arity) :-
    history_predicate(Name),
    !,
    fail.
repl_user_predicate(SessionId, Name, Arity) :-
    % User DML clauses are asserted (interpreted) into the session module.
    % Exclude predicates imported via use_module (e.g. library(clpfd)) and
    % foreign/builtin predicates. Build a callable head rather than a predicate
    % indicator: predicate_property/2 wants a goal (Module:Goal).
    functor(Head, Name, Arity),
    predicate_property(SessionId:Head, interpreted),
    \+ predicate_property(SessionId:Head, imported_from(_)),
    \+ predicate_property(SessionId:Head, foreign).

internal_session_predicate(param).
internal_session_predicate(param_decl).
internal_session_predicate(tool_source).
internal_session_predicate('__session__').
internal_session_predicate(Name) :- workspace_predicate_name(Name).

%% history_predicate(+Name)
%% Runtime-owned harness-history facts. Excluded from the user predicate set (so
%% they are neither listed as harness predicates nor exported nor replaced by a
%% commit) and protected from user definition -- but CALLABLE from DML, so the
%% harness can query what happened in previous sessions.
history_predicate(Name) :-
    atom(Name),
    sub_atom(Name, 0, _, _, harness_).

%% repl_reserved_name(+Name)
%% Names owned by the REPL runtime. M4 uses repl_reserved/1 to refuse goals
%% that try to re-enter or shadow the REPL control surface.
%% Any repl_*/meta_* name is reserved (namespace guard), plus the explicit
%% control names below. repl_delete is a protected exception for definition
%% purposes (see repl_protected_name/1).
repl_reserved_name(Name) :-
    atom(Name),
    (   sub_atom(Name, 0, _, _, repl_)
    ;   sub_atom(Name, 0, _, _, meta_)
    ).

repl_reserved_name(repl_meta_prompt).
repl_reserved_name(repl_extract_code).
repl_reserved_name(repl_reserved).
repl_reserved_name(repl_reserved_name).
repl_reserved_name(repl_tool_catalog).
repl_reserved_name(repl_listing).
repl_reserved_name(repl_assert).
repl_reserved_name(require_sandbox).
repl_reserved_name(repl_retract).
repl_reserved_name(repl_delete).
repl_reserved_name(repl_freeze).
repl_reserved_name(repl_unfreeze).
repl_reserved_name(repl_is_frozen).
repl_reserved_name(repl_solve).
repl_reserved_name(repl_initial_state).
repl_reserved_name(repl_user_predicate).
repl_reserved_name(query_start).
repl_reserved_name(read_repl_goal).
repl_reserved_name(create_session).
repl_reserved_name(clear_session).
repl_reserved_name(destroy_session).
repl_reserved_name(create_frame).
repl_reserved_name(create_frame_raw).
repl_reserved_name(step_frame).
repl_reserved_name(end_frame).
repl_reserved_name(frame_info).
repl_reserved_name(default_frame).
repl_reserved_name(load_code).
repl_reserved_name(freeze).
repl_reserved_name(unfreeze).
repl_reserved_name(reset).
repl_reserved_name(export).

%% repl_reserved(+TermOrNameArity)
repl_reserved(Name) :-
    atom(Name),
    repl_reserved_name(Name).
repl_reserved(Name/Arity) :-
    atom(Name),
    integer(Arity),
    repl_reserved_name(Name).
repl_reserved(Term) :-
    compound(Term),
    functor(Term, Name, _),
    repl_reserved_name(Name).

%% ============================================================
%% Freeze Lock and Export (M2)
%% ============================================================

%% repl_freeze(+SessionId)
%% Makes the living module read-only. Queries, `execute`, and nested task()
%% calls still work; only mutation is refused.
repl_freeze(SessionId) :-
    retractall(session_frozen(SessionId, _)),
    assertz(session_frozen(SessionId, true)).

%% repl_lock(+SessionId)
%% Irreversibly freezes: repl_unfreeze/1 refuses. Used by --locked.
repl_lock(SessionId) :-
    repl_freeze(SessionId),
    retractall(session_locked(SessionId, _)),
    assertz(session_locked(SessionId, true)).

%% repl_unfreeze(+SessionId)
repl_unfreeze(SessionId) :-
    (   session_locked(SessionId, true)
    ->  throw(error(permission_error(modify, locked_harness, SessionId),
                context(repl_unfreeze/1, 'harness is locked; unfreeze is disabled')))
    ;   retractall(session_frozen(SessionId, _)),
        assertz(session_frozen(SessionId, false))
    ).

%% repl_is_frozen(+SessionId)
repl_is_frozen(SessionId) :-
    session_frozen(SessionId, true),
    !.

%% repl_try_unfreeze(+SessionId, -Error)
%% Non-throwing wrapper so the host can report a locked harness cleanly.
repl_try_unfreeze(SessionId, Error) :-
    catch(repl_unfreeze(SessionId), UnfreezeError,
        format(atom(Error), '~w', [UnfreezeError])),
    (   var(Error)
    ->  Error = none
    ;   true
    ).

%% guard_not_frozen(+SessionId)
%% Shared mutation guard for assert/retract/consult/load.
guard_not_frozen(SessionId) :-
    (   session_frozen(SessionId, true)
    ->  throw(error(permission_error(modify, frozen_harness, SessionId),
                context(mi_call/3, 'harness is frozen; assert/retract/load are disabled')))
    ;   true
    ).

%% record_directive(+SessionId, +Directive)
record_directive(SessionId, Directive) :-
    (   session_directive(SessionId, Directive)
    ->  true
    ;   assertz(session_directive(SessionId, Directive))
    ).

%% ---- Export (serialization) ----

%% export_harness(+SessionId, +Header, -Text)
%% Deterministically serialize the living module into self-contained DML:
%% directives, tools, embedded scripts, then user predicates sorted name/arity.
export_harness(SessionId, Header, Text) :-
    with_output_to(string(Text), export_harness_output(SessionId, Header)).

export_harness_output(SessionId, Header) :-
    (   Header == none
    ->  true
    ;   format("% ~w~n", [Header])
    ),
    export_directives(SessionId),
    export_tools(SessionId),
    export_clauses(SessionId).

export_directives(SessionId) :-
    findall(Directive, session_directive(SessionId, Directive), Directives0),
    sort(Directives0, Directives),
    forall(member(Directive, Directives), format(":- ~q.~n", [Directive])),
    (   Directives == []
    ->  true
    ;   nl
    ).

export_tools(SessionId) :-
    findall(Name, session_user_tool_schema(SessionId, Name, _), Names0),
    sort(Names0, Names),
    forall(member(Name, Names),
        (   SessionId:tool_source(Name, Source)
        ->  format("~w~n~n", [Source])
        ;   true
        )).

export_clauses(SessionId) :-
    findall(Name/Arity,
        (   current_predicate(SessionId:Name/Arity),
            export_user_predicate(SessionId, Name, Arity)
        ),
        Preds0),
    sort(Preds0, Preds),
    forall(member(Name/Arity, Preds),
        (   functor(Head, Name, Arity),
            forall(clause(SessionId:Head, Body), export_clause(Head, Body))
        )).

export_user_predicate(SessionId, Name, Arity) :-
    repl_user_predicate(SessionId, Name, Arity),
    \+ tool_predicate_name(Name).

%% export_clause(+Head, +Body)
%% Copy + numbervars the clause so variable names are deterministic (A, B, …),
%% which is required for byte-stable export → reload → export.
%% export_stats(+SessionId, -Clauses, -Tools, -Scripts)
export_stats(SessionId, Clauses, Tools, Scripts) :-
    findall(Name/Arity,
        (   current_predicate(SessionId:Name/Arity),
            export_user_predicate(SessionId, Name, Arity)
        ),
        Preds0),
    sort(Preds0, Preds),
    findall(Count,
        (   member(PredName/PredArity, Preds),
            functor(PredHead, PredName, PredArity),
            aggregate_all(count, clause(SessionId:PredHead, _), Count)
        ),
        PredCounts),
    sum_list(PredCounts, Clauses),
    findall(Name, session_user_tool_schema(SessionId, Name, _), ToolNames0),
    sort(ToolNames0, ToolNames),
    length(ToolNames, Tools),
    (   current_predicate(SessionId:script_file/2)
    ->  findall(Path, SessionId:script_file(_, Path), ScriptPaths0),
        sort(ScriptPaths0, ScriptPaths),
        length(ScriptPaths, Scripts)
    ;   Scripts = 0
    ).

export_clause(Head, Body) :-
    print_user_clause(Head, Body),
    nl.

%% format_tool_source(+Head, +Description, +Body, -SourceCode)
%% Source text for a tool, used by `listing(tools)` and export. Reverses the
%% load-time task/prompt rewrites so readers see `task/N`, not `task_named/3`.
format_tool_source(Head, none, Body, SourceCode) :-
    !,
    copy_term(Head-Body, NH-NB),
    numbervars(NH-NB, 0, _),
    clean_clause_body(NB, CleanBody),
    (   CleanBody == true
    ->  format(string(SourceCode), "tool(~q).", [NH])
    ;   format(string(SourceCode), "tool(~q) :-~n    ~q.", [NH, CleanBody])
    ).
format_tool_source(Head, Description, Body, SourceCode) :-
    copy_term(Head-Description-Body, NH-ND-NB),
    numbervars(NH-ND-NB, 0, _),
    clean_clause_body(NB, CleanBody),
    (   CleanBody == true
    ->  format(string(SourceCode), "tool(~q, ~q).", [NH, ND])
    ;   format(string(SourceCode), "tool(~q, ~q) :-~n    ~q.", [NH, ND, CleanBody])
    ).

%% print_user_clause(+Head, +Body)
%% Copy + numbervars for deterministic variable names, reverse the load-time
%% task/prompt rewrites (see clean_clause_body/2), then print in source form.
print_user_clause(Head, Body) :-
    copy_term(Head-Body, NumberedHead-NumberedBody),
    numbervars(NumberedHead-NumberedBody, 0, _),
    clean_clause_body(NumberedBody, CleanBody),
    (   CleanBody == true
    ->  format("~q.~n", [NumberedHead])
    ;   format("~q :-~n    ~q.~n", [NumberedHead, CleanBody])
    ).

tool_predicate_name(tool).
tool_predicate_name(tool_source).

%% repl_script_files(+SessionId, -Paths)
%% Workspace-relative files linked by script_file/2 facts; the host bundles them.
repl_script_files(SessionId, Paths) :-
    (   current_predicate(SessionId:script_file/2)
    ->  % Only literal paths count; a stray variable path is ignored.
        findall(Path,
            (   SessionId:script_file(_, Path),
                (atom(Path) ; string(Path))
            ),
            Paths0),
        sort(Paths0, Paths)
    ;   Paths = []
    ).

%% repl_materialize_embedded(+SessionId)
%% Writes every embedded_file(Path, Content) fact to its workspace path. Used
%% after loading a single-file .dml export (where linked scripts are inlined).
repl_materialize_embedded(SessionId) :-
    (   current_predicate(SessionId:embedded_file/2)
    ->  forall(SessionId:embedded_file(Path, Content),
            (   catch(
                    (   file_directory_name(Path, Dir),
                        make_directory_path(Dir),
                        setup_call_cleanup(
                            open(Path, write, Stream),
                            format(Stream, "~s", [Content]),
                            close(Stream))
                    ),
                    _,
                    true
                )
            ))
    ;   true
    ).

%% ============================================================
%% Prompt Mode (M3)
%% ============================================================

%% repl_protected_name(+Name)
%% Reserved names that generated code may not (re)define. repl_delete is a
%% legitimate harness directive and is allowed through.
repl_protected_name(Name) :-
    history_predicate(Name).
repl_protected_name(Name) :-
    repl_reserved_name(Name),
    Name \== repl_delete.

repl_protected_clause((Head :- _)) :-
    callable(Head),
    functor(Head, Name, _),
    repl_protected_name(Name).
repl_protected_clause(Head) :-
    callable(Head),
    functor(Head, Name, _),
    repl_protected_name(Name).

%% repl_set_meta_catalog(+SessionId, +CatalogText)
repl_set_meta_catalog(SessionId, CatalogText) :-
    retractall(session_meta_catalog(SessionId, _)),
    assertz(session_meta_catalog(SessionId, CatalogText)).

%% repl_prepare_single(+SessionId, +ClauseText, -PreparedTerms)
%% Reads exactly ONE source term and runs the SAME load-time preprocessing as
%% parse_dml_string/2: transform_task_calls/3 (task/N, prompt/N ->
%% task_named/3, prompt_named/3), expand_interpolations/3 ({Var} strings), and
%% expand_term/2 (e.g. DCG rules). Scratch asserts must be preprocessed exactly
%% like a file load, or a clause asserted mid-turn keeps task/N, which has no
%% runtime handler and fails silently.
%%
%% Trailing content is an error so `assert` can never silently drop clauses that
%% `assert_many` would have kept.
repl_prepare_single(SessionId, ClauseText, PreparedTerms) :-
    ensure_full_stop(ClauseText, Text1),
    open_string(Text1, Stream),
    read_term(Stream, Term, [module(SessionId), variable_names(Bindings)]),
    (   read_term(Stream, Extra, [module(SessionId)]),
        Extra \== end_of_file
    ->  close(Stream),
        throw(error(syntax_error('assert expects a single clause; use assert_many for multiple'), _))
    ;   close(Stream)
    ),
    preprocess_loaded_term(Term, Bindings, PreparedTerms).

%% repl_read_clause(+SessionId, +ClauseText, -Clause)
%% Reads exactly one term without preprocessing. Used by retract, which only
%% needs the predicate's name/arity. Trailing content is an error.
repl_read_clause(SessionId, ClauseText, Clause) :-
    ensure_full_stop(ClauseText, Text1),
    open_string(Text1, Stream),
    read_term(Stream, Clause, [module(SessionId)]),
    (   read_term(Stream, Extra, [module(SessionId)]),
        Extra \== end_of_file
    ->  close(Stream),
        throw(error(syntax_error('retract expects a single clause pattern'), _))
    ;   close(Stream)
    ).

%% repl_head_of(+Term, -Head)
%% Strips a rule body, so retracting with a full clause pattern targets the
%% predicate instead of accidentally matching every rule ((:-)/2).
repl_head_of((Head :- _Body), Head) :- !.
repl_head_of(Head, Head).

%% repl_assert_clause(+PreparedTerm, +SessionId)
%% Asserts one already-preprocessed term, rejecting reserved names.
repl_assert_clause(PreparedTerm, SessionId) :-
    (   repl_protected_clause(PreparedTerm)
    ->  throw(error(permission_error(modify, reserved_name, PreparedTerm),
                context(repl_assert/3, 'cannot define a reserved predicate')))
    ;   process_clause(PreparedTerm, SessionId)
    ).

%% repl_assert(+SessionId, +ClauseText, -Result)
%% Parses and asserts generated DML with the load-time preprocessing applied,
%% so scratch-asserted clauses behave exactly like loaded ones.
%% Refused when frozen; reserved names are rejected.
repl_assert(SessionId, ClauseText, Result) :-
    catch(
        (   guard_not_frozen(SessionId),
            repl_prepare_single(SessionId, ClauseText, PreparedTerms),
            forall(member(PreparedTerm, PreparedTerms),
                repl_require_new_predicate(SessionId, PreparedTerm)),
            forall(member(PreparedTerm, PreparedTerms),
                repl_assert_clause(PreparedTerm, SessionId)),
            Result = ok
        ),
        AssertError,
        format(atom(Result), 'error(~w)', [AssertError])
    ).

%% repl_set_dml_reference(+SessionId, +Text)
%% Stores the bundled DML reference so the meta prompt can include it.
repl_set_dml_reference(SessionId, Text) :-
    retractall(session_dml_reference(SessionId, _)),
    assertz(session_dml_reference(SessionId, Text)).

%% repl_set_authoring_examples(+SessionId, +Text)
%% Stores the curated authoring patterns for the meta prompt.
repl_set_authoring_examples(SessionId, Text) :-
    retractall(session_authoring_examples(SessionId, _)),
    assertz(session_authoring_examples(SessionId, Text)).

%% repl_set_meta_template(+SessionId, +Template)
%% Stores the meta-prompt template (the bundled META_PROMPT.md). Placeholders
%% {{CATALOG}}, {{REFERENCE}}, {{EXAMPLES}}, {{MANIFEST}}, {{MEMORY}}, {{REQUEST}}
%% are filled by repl_meta_prompt/3.
repl_set_meta_template(SessionId, Template) :-
    retractall(session_meta_template(SessionId, _)),
    assertz(session_meta_template(SessionId, Template)).

%% repl_set_harness_examples(+SessionId, +Text)
%% Stores the bundled HARNESS_EXAMPLES.md ({{EXAMPLES}}).
repl_set_harness_examples(SessionId, Text) :-
    retractall(session_harness_examples(SessionId, _)),
    assertz(session_harness_examples(SessionId, Text)).

%% repl_set_prolog_snippets(+SessionId, +Text)
%% Stores the bundled PROLOG_SNIPPETS.md ({{SNIPPETS}}).
repl_set_prolog_snippets(SessionId, Text) :-
    retractall(session_prolog_snippets(SessionId, _)),
    assertz(session_prolog_snippets(SessionId, Text)).

%% repl_parse_clauses(+SessionId, +ClausesText, -Terms)
%% Parse and preprocess a block of DML clauses without asserting them.
repl_parse_clauses(SessionId, ClausesText, Terms) :-
    ensure_full_stop(ClausesText, Text1),
    open_string(Text1, Stream),
    repl_read_prepared_terms(Stream, SessionId, Terms),
    close(Stream).

repl_read_prepared_terms(Stream, SessionId, Terms) :-
    read_term(Stream, Term, [module(SessionId), variable_names(Bindings)]),
    (   Term == end_of_file
    ->  Terms = []
    ;   % Apply the same preprocessing as parse_dml_string/2 so scratch clauses
        % match loaded ones (task/prompt rewrite, interpolation, expand_term).
        preprocess_loaded_term(Term, Bindings, Prepared),
        % Directives (e.g. use_module(library(clpfd))) must run while parsing so
        % their operators are visible to the terms that follow; keep them out of
        % the returned list so repl_commit_terms does not process them twice.
        partition_directives(Prepared, Rules, Directives),
        forall(member(Directive, Directives), repl_commit_directive(Directive, SessionId)),
        append(Rules, Rest, Terms),
        repl_read_prepared_terms(Stream, SessionId, Rest)
    ).

%% repl_commit_directive(+Directive, +SessionId)
%% Directives written by the *agent* (assert_many/define) run here. `require_sandbox`
%% is the user's to set — through `/set-require-sandbox` — so an agent cannot
%% declare a requirement it was not asked for, nor lift one by re-asserting the
%% harness. The file-load path (parse_clauses/2) is deliberately not guarded:
%% that is what the user's own harness file goes through.
repl_commit_directive((:- require_sandbox), _SessionId) :-
    !,
    throw(error(permission_error(modify, reserved_name, require_sandbox),
                context(repl_commit_directive/2,
                        'only the user can set the sandbox requirement'))).
repl_commit_directive(Directive, SessionId) :-
    process_clause(Directive, SessionId).

partition_directives([], [], []).
partition_directives([Term|Terms], Rules, Directives) :-
    (   Term = (:- _)
    ->  Rules = Rules0,
        Directives = [Term|Directives0]
    ;   Rules = [Term|Rules0],
        Directives = Directives0
    ),
    partition_directives(Terms, Rules0, Directives0).

%% repl_commit_terms(+SessionId, +Terms)
%% Assert already-preprocessed terms (the shared tail of assert_many/define).
repl_commit_terms(SessionId, Terms) :-
    forall(member(PreparedTerm, Terms),
        (   repl_protected_clause(PreparedTerm)
        ->  throw(error(permission_error(modify, reserved_name, PreparedTerm),
                    context(repl_assert_many/3, 'cannot define a reserved predicate')))
        ;   process_clause(PreparedTerm, SessionId)
        )).

%% repl_assert_many(+SessionId, +ClausesText, -Result)
%% Parses a block of clauses and asserts them in the order written. Prefer this
%% for defining a predicate: clause order matters for backtracking, so asserting
%% one clause at a time can produce a wrong program.
repl_assert_many(SessionId, ClausesText, Result) :-
    catch(
        (   guard_not_frozen(SessionId),
            repl_parse_clauses(SessionId, ClausesText, Terms),
            % assert appends; against an existing predicate that leaves both
            % versions live with their clauses interleaved.
            forall(member(Term, Terms), repl_require_new_predicate(SessionId, Term)),
            repl_commit_terms(SessionId, Terms),
            repl_note_touched(SessionId, Terms),
            Result = ok
        ),
        AssertError,
        format(atom(Result), 'error(~w)', [AssertError])
    ).

%% repl_term_indicator(+Term, -Name/Arity)
%% The plain predicate a parsed clause defines. Directives and tool/2 wrappers
%% have no plain indicator here (process_clause owns them).
repl_term_indicator((:- _), _) :- !, fail.
repl_term_indicator((tool(_, _) :- _), _) :- !, fail.
repl_term_indicator((tool(_) :- _), _) :- !, fail.
repl_term_indicator(Term, Name/Arity) :-
    repl_head_of(Term, Head),
    callable(Head),
    functor(Head, Name, Arity).

%% repl_note_touched(+SessionId, +Terms)
%% Record the predicate indicators a term list defines, for history recording.
repl_note_touched(SessionId, Terms) :-
    forall(( member(Term, Terms), repl_term_indicator(Term, Name/Arity) ),
        repl_note_indicator(SessionId, Name/Arity)).

repl_note_indicator(SessionId, Name/Arity) :-
    (   session_edit_touched(SessionId, Name/Arity)
    ->  true
    ;   assertz(session_edit_touched(SessionId, Name/Arity))
    ).

%% repl_require_new_predicate(+SessionId, +Term)
%% `assert`/`assert_many` append. Against a predicate that already has clauses
%% that mixes the old and the new version — both stay live, clause order
%% interleaves them, and nothing says so. Refuse instead, and name the remedy.
repl_require_new_predicate(SessionId, Term) :-
    repl_term_indicator(Term, Name/Arity),
    findall(x,
        (   functor(Head, Name, Arity),
            clause(SessionId:Head, _)
        ),
        Existing),
    Existing \== [],
    !,
    length(Existing, Count),
    format(atom(Message),
        '~w/~w already has ~w clause(s). assert appends, so the old and the new clauses would both be present and clause order would interleave them. Use define("~w/~w", "...") to replace the predicate, or delete("~w/~w") first.',
        [Name, Arity, Count, Name, Arity, Name, Arity]),
    throw(error(permission_error(modify, existing_predicate, Name/Arity),
                context(repl_require_new_predicate/2, Message))).
repl_require_new_predicate(_SessionId, _Term).

%% repl_forget_tool(+SessionId, +Name)
%% Drop the tool annotation that belonged to a predicate being removed.
repl_forget_tool(SessionId, Name) :-
    (   session_user_tools(SessionId, Name)
    ->  retractall(session_user_tools(SessionId, Name)),
        retractall(session_user_tool_schema(SessionId, Name, _)),
        catch(retractall(SessionId:tool_source(Name, _)), _, true)
    ;   true
    ).

%% repl_capture_predicates(+SessionId, +Indicators, -Captured)
%% Copy the clauses and tool bookkeeping a replacement is about to destroy, so
%% a failure part-way through can put them back.
repl_capture_predicates(SessionId, Indicators, Captured) :-
    findall(indicator(Name, Arity, Clauses, tool(IsTool, Schema, Source)),
        (   member(Name/Arity, Indicators),
            findall(Stored,
                (   functor(Head, Name, Arity),
                    clause(SessionId:Head, Body),
                    (   Body == true
                    ->  Stored = Head
                    ;   Stored = (Head :- Body)
                    )
                ),
                Clauses),
            (   session_user_tools(SessionId, Name)
            ->  IsTool = true
            ;   IsTool = false
            ),
            (   session_user_tool_schema(SessionId, Name, Found)
            ->  Schema = Found
            ;   Schema = none
            ),
            (   current_predicate(SessionId:tool_source/2),
                SessionId:tool_source(Name, FoundSource)
            ->  Source = FoundSource
            ;   Source = none
            )
        ),
        Captured).

%% repl_restore_captured(+SessionId, +Captured)
repl_restore_captured(SessionId, Captured) :-
    forall(member(indicator(Name, Arity, Clauses, tool(IsTool, Schema, Source)), Captured),
        (   catch(abolish(SessionId:Name/Arity), _, true),
            repl_forget_tool(SessionId, Name),
            forall(member(Clause, Clauses), assertz(SessionId:Clause)),
            (   IsTool == true
            ->  assertz(session_user_tools(SessionId, Name))
            ;   true
            ),
            (   Schema == none
            ->  true
            ;   assertz(session_user_tool_schema(SessionId, Name, Schema))
            ),
            (   Source == none
            ->  true
            ;   assertz(SessionId:tool_source(Name, Source))
            )
        )).

%% repl_replace_predicates(+SessionId, +Indicators, +Terms)
%% The destructive half of define: abolish the targets, then assert the block.
repl_replace_predicates(SessionId, Indicators, Terms) :-
    forall(member(Name/Arity, Indicators),
        (   catch(abolish(SessionId:Name/Arity), _, true),
            repl_forget_tool(SessionId, Name)
        )),
    repl_commit_terms(SessionId, Terms).

%% repl_edit_touched(+SessionId, -Indicators)
%% Predicates touched by edit tools since the last clear (sorted, unique).
repl_edit_touched(SessionId, Indicators) :-
    findall(Name/Arity, session_edit_touched(SessionId, Name/Arity), Raw),
    sort(Raw, Indicators).

%% repl_edit_touched_text(+SessionId, -Text)
%% "Name/Arity, Name/Arity" of the predicates an agent turn touched.
repl_edit_touched_text(SessionId, Text) :-
    repl_edit_touched(SessionId, Indicators),
    with_output_to(string(Text), print_pred_list(Indicators)).

repl_edit_touched_clear(SessionId) :-
    retractall(session_edit_touched(SessionId, _)).

%% repl_callable_indicator(+Head, -Name, -Arity)
%% Validate a predicate head: an ordinary (non-operator, non-system) functor.
repl_callable_indicator(Head, Name, Arity) :-
    callable(Head),
    functor(Head, Name, Arity),
    atom(Name),
    Name \== '[]',
    Name \== '{}',
    \+ repl_system_functor(Name/Arity).

%% Functors that are list/operator/system structure, never user predicates.
repl_system_functor(Name/_) :-
    member(Name, ['.', ',', ';', ':-', '->', ':', '|', '!', '\\+', '=', 'is']).

%% repl_retract(+SessionId, +ClausePattern, -Result)
%% Removes the clauses that match the pattern, and says how many.
%%
%% This used to build a fresh head with `functor/3` and call `retractall/1`, so
%% the pattern's *arguments were discarded*: `retract("g(1)")` removed every
%% g/1 clause and reported `ok`. It now retracts what was asked for, and fails
%% loudly when nothing matched — a silent no-op is how an agent concludes a
%% clause is gone when it never was.
repl_retract(SessionId, ClauseText, Result) :-
    catch(
        (   guard_not_frozen(SessionId),
            repl_read_clause(SessionId, ClauseText, Pattern),
            repl_head_of(Pattern, Clause),
            repl_callable_indicator(Clause, Name, Arity),
            (   repl_protected_clause(Clause)
            ->  throw(error(permission_error(modify, reserved_name, Clause),
                        context(repl_retract/3, 'cannot retract a reserved predicate')))
            ;   true
            ),
            findall(x, retract(SessionId:Pattern), Removed),
            length(Removed, RemovedCount),
            (   RemovedCount =:= 0
            ->  format(atom(Message),
                    'no clause of ~w/~w matched ~w, so nothing was removed',
                    [Name, Arity, Clause]),
                throw(error(existence_error(clause, Clause),
                            context(repl_retract/3, Message)))
            ;   true
            ),
            % Only drop the tool annotation when the predicate is really gone;
            % other clauses of it may still be live.
            (   functor(Head, Name, Arity),
                clause(SessionId:Head, _)
            ->  true
            ;   repl_forget_tool(SessionId, Name)
            ),
            repl_note_indicator(SessionId, Name/Arity),
            (   RemovedCount =:= 1
            ->  Suffix = ''
            ;   Suffix = 's'
            ),
            format(atom(Result), 'ok(removed ~w clause~w)', [RemovedCount, Suffix])
        ),
        RetractError,
        format(atom(Result), 'error(~w)', [RetractError])
    ).

%% repl_delete_predicate(+SessionId, +Name, +Arity, -Result)
%% Fully remove a predicate. Unlike retract (which only removes matching
%% clauses and can leave an empty predicate behind), this abolishes it.
repl_delete_predicate(SessionId, Name, Arity, Result) :-
    catch(
        (   guard_not_frozen(SessionId),
            atom(Name), integer(Arity), Arity >= 0,
            (   repl_protected_name(Name)
            ->  throw(error(permission_error(modify, reserved_name, Name/Arity),
                        context(repl_delete_predicate/4, 'cannot delete a reserved predicate')))
            ;   true
            ),
            catch(abolish(SessionId:Name/Arity), _, true),
            (   session_user_tools(SessionId, Name)
            ->  retractall(session_user_tools(SessionId, Name)),
                retractall(session_user_tool_schema(SessionId, Name, _)),
                retractall(SessionId:tool_source(Name, _))
            ;   true
            ),
            (   session_edit_touched(SessionId, Name/Arity)
            ->  true
            ;   assertz(session_edit_touched(SessionId, Name/Arity))
            ),
            Result = ok
        ),
        DeleteError,
        format(atom(Result), 'error(~w)', [DeleteError])
    ).

%% repl_define(+SessionId, +HeadPatternText, +ClausesText, -Result)
%% Replace every predicate the clause block defines, all or nothing.
%%
%% The order matters and used to be wrong. Abolishing first and asserting after
%% means any failure in between leaves the target *destroyed*: a block with one
%% reserved name in it removed the predicate it was replacing and reported an
%% error, and a following `end_turn` then committed the deletion as a version.
%% So everything that can fail is checked before anything is touched, and the
%% clauses being replaced are copied first so a failure can still put them back.
repl_define(SessionId, HeadPatternText, ClausesText, Result) :-
    catch(
        (   guard_not_frozen(SessionId),
            repl_define_target(SessionId, HeadPatternText, HeadName, HeadArity),
            repl_parse_clauses(SessionId, ClausesText, Terms),
            findall(Name/Arity,
                ( member(Term, Terms), repl_term_indicator(Term, Name/Arity) ),
                BlockIndicators0),
            sort(BlockIndicators0, BlockIndicators),
            % The head names the predicate being replaced. If it is not one of
            % them the target is almost certainly a typo, and silently replacing
            % a *different* predicate is worse than refusing.
            (   BlockIndicators \== [],
                \+ member(HeadName/HeadArity, BlockIndicators)
            ->  format(atom(TargetMessage),
                    'define was told to replace ~w/~w, but the clause block defines ~w. Pass the predicate you are replacing as the head.',
                    [HeadName, HeadArity, BlockIndicators]),
                throw(error(domain_error(define_target, HeadName/HeadArity),
                            context(repl_define/4, TargetMessage)))
            ;   true
            ),
            sort([HeadName/HeadArity|BlockIndicators], Indicators),
            % Validate everything that can fail, before anything is removed.
            forall(member(Term, Terms), repl_require_committable(SessionId, Term)),
            forall(member(Name/Arity, Indicators), repl_require_not_reserved(SessionId, Name/Arity)),
            repl_capture_predicates(SessionId, Indicators, Captured),
            (   catch(repl_replace_predicates(SessionId, Indicators, Terms), ReplaceError,
                    (   repl_restore_captured(SessionId, Captured),
                        throw(ReplaceError)
                    ))
            ),
            repl_note_touched(SessionId, Terms),
            Result = ok
        ),
        DefineError,
        format(atom(Result), 'error(~w)', [DefineError])
    ).

%% repl_define_target(+SessionId, +HeadText, -Name, -Arity)
%% The first argument of `define` names the predicate being replaced, written as
%% an indicator (`f/1`) or as a clause head (`f(_)`).
%%
%% It has to be decomposed explicitly: `f/1` *reads* as the operator term
%% `/(f, 1)`, so treating it as a clause head yields the functor `//2`. That is
%% what used to happen — the target was silently ignored and the clause block
%% alone decided what got replaced, so `define` accepted any name at all.
repl_define_target(SessionId, HeadText, Name, Arity) :-
    repl_read_clause(SessionId, HeadText, Pattern),
    (   Pattern = IndicatorName/IndicatorArity,
        atom(IndicatorName),
        integer(IndicatorArity),
        IndicatorArity >= 0
    ->  Name = IndicatorName,
        Arity = IndicatorArity
    ;   repl_head_of(Pattern, HeadClause),
        repl_callable_indicator(HeadClause, Name, Arity)
    ).

%% repl_require_committable(+SessionId, +Term)
repl_require_committable(_SessionId, Term) :-
    (   repl_protected_clause(Term)
    ->  throw(error(permission_error(modify, reserved_name, Term),
                context(repl_define/4, 'cannot define a reserved predicate')))
    ;   true
    ).

%% repl_require_not_reserved(+SessionId, +Indicator)
repl_require_not_reserved(_SessionId, Name/Arity) :-
    (   repl_protected_name(Name)
    ->  throw(error(permission_error(modify, reserved_name, Name/Arity),
                context(repl_define/4, 'cannot define a reserved predicate')))
    ;   true
    ).

%% repl_extract_code(+Text, -Source)
%% Extracts a ```dml fenced block; falls back to the whole answer.
repl_extract_code(Text, Source) :-
    atom_string(TextAtom, Text),
    (   sub_string(TextAtom, Start0, _, _, "```dml")
    ->  Start is Start0 + 6,
        % After = 0 takes the remainder of the text (an unbound After would
        % bind Rest to the empty prefix first).
        sub_string(TextAtom, Start, _, 0, Rest),
        % Search for the closing fence by offset: binding Before=0 here would
        % require the fence to be at the very start of Rest.
        (   sub_string(Rest, FenceEnd, _, _, "```")
        ->  sub_string(Rest, 0, FenceEnd, _, Source)
        ;   Source = Rest
        )
    ;   Source = TextAtom
    ).

%% repl_manifest(+SessionId, -ManifestText)
%% Compact signatures + tools + entries + directives for the meta prompt.
repl_manifest(SessionId, Manifest) :-
    findall(Name/Arity,
        (   current_predicate(SessionId:Name/Arity),
            repl_user_predicate(SessionId, Name, Arity)
        ),
        Preds0),
    sort(Preds0, Preds),
    findall(Name, session_user_tool_schema(SessionId, Name, _), Tools0),
    sort(Tools0, Tools),
    findall(Directive, session_directive(SessionId, Directive), Directives0),
    sort(Directives0, Directives),
    (   current_predicate(SessionId:module_doc/1),
        SessionId:module_doc(Summary0)
    ->  Summary = Summary0
    ;   Summary = ""
    ),
    (   current_predicate(SessionId:doc/2)
    ->  findall(PI-DocText, SessionId:doc(PI, DocText), Docs0)
    ;   Docs0 = []
    ),
    repl_history_summary(SessionId, HistorySummary),
    with_output_to(string(Manifest),
        (   (   Summary == ""
            ->  format("- skill: (none; add module_doc/1 so the harness describes itself and can be routed)~n")
            ;   format("- skill: ~w~n", [Summary])
            ),
            format("- predicates: ~w~n", [Preds]),
            format("- tools: ~w~n", [Tools]),
            format("- directives: ~w~n", [Directives]),
            (   Docs0 == []
            ->  format("- docs: (none; add doc(Name/Arity, Text) facts)~n")
            ;   forall(member(PI-DocText, Docs0), format("- doc(~w): ~w~n", [PI, DocText]))
            ),
            format("- history: ~w~n", [HistorySummary])
        )).

%% repl_meta_memory_text(+SessionId, -Text)
repl_meta_memory_text(SessionId, Text) :-
    (   session_meta_memory(SessionId, Memory)
    ->  true
    ;   Memory = []
    ),
    last_n(Memory, 10, Recent),
    with_output_to(string(Text),
        forall(member(message{role: Role, content: Content}, Recent),
            format("~w: ~w~n", [Role, Content]))).

%% repl_memory_term(+SessionId, +Slot, -TermText)
%% Serializes a live memory slot (harness|meta) as a Prolog term so a session
%% can persist it and restore it on `--session` resume.
repl_memory_term(SessionId, Slot, Text) :-
    (   Slot == meta
    ->  (   session_meta_memory(SessionId, Memory)
        ->  true
        ;   Memory = []
        )
    ;   (   session_repl_memory(SessionId, Memory)
        ->  true
        ;   Memory = []
        )
    ),
    with_output_to(string(Text),
        write_term(Memory, [quoted(true), max_depth(0)])).

last_n(List, N, Tail) :-
    length(List, Len),
    (   Len =< N
    ->  Tail = List
    ;   Skip is Len - N,
        length(Prefix, Skip),
        append(Prefix, Tail, List)
    ).

%% ============================================================
%% Harness history (see docs/DML_REPL_SESSION_HISTORY.md)
%% ============================================================

%% repl_history_load(+SessionId, +Text)
%% Asserts harness_* history facts (the persisted .deepclause/repl/history.dml)
%% into the session module so the harness can query prior sessions.
repl_history_load(SessionId, Text) :-
    (   var(Text)
    ->  true
    ;   open_string(Text, Stream),
        repl_history_read_stream(Stream, SessionId),
        close(Stream)
    ).

repl_history_read_stream(Stream, SessionId) :-
    catch(read_term(Stream, Term, [module(SessionId)]), _, Term = end_of_file),
    (   Term == end_of_file
    ->  true
    ;   repl_history_assert_term(Term, SessionId),
        repl_history_read_stream(Stream, SessionId)
    ).

%% repl_history_record(+SessionId, +FactText)
%% Asserts one or more history facts into the live module. The recorder persists
%% the same facts to history.dml; here they become queryable.
repl_history_record(SessionId, FactText) :-
    (   var(FactText)
    ->  true
    ;   ensure_full_stop(FactText, Text1),
        open_string(Text1, Stream),
        repl_history_read_stream(Stream, SessionId),
        close(Stream)
    ).

%% Only harness_* facts may be asserted through this path.
repl_history_assert_term(Term, SessionId) :-
    (   nonvar(Term),
        Term \== end_of_file,
        Term \== none,
        callable(Term),
        functor(Term, Name, _),
        history_predicate(Name)
    ->  % harness_head is a singleton: keep the latest (rollback moves it back).
        (   Name == harness_head
        ->  retractall(SessionId:harness_head(_, _, _, _))
        ;   true
        ),
        % Migrate pre-/5 harness_change facts (they carried an unused Entry arg).
        (   Term = harness_change(V, Ts, Kind, Sid, Turn, _Entry)
        ->  assertz(SessionId:harness_change(V, Ts, Kind, Sid, Turn))
        ;   assertz(SessionId:Term)
        )
    ;   true
    ).

%% repl_rollback(+SessionId, +Source, -Result)
%% Restores the living module from a history version's serialized source:
%% abolishes the user predicates and reloads the source (directives, tools and
%% entries come from the source too). Refused when frozen. Runtime conversation
%% memory is preserved.
repl_rollback(SessionId, Source, Result) :-
    catch(
        (   guard_not_frozen(SessionId),
            repl_abolish_user_predicates(SessionId),
            retractall(session_directive(SessionId, _)),
            retractall(session_user_tools(SessionId, _)),
            retractall(session_user_tool_schema(SessionId, _, _)),
            (   current_predicate(SessionId:tool_source/2)
            ->  retractall(SessionId:tool_source(_, _))
            ;   true
            ),
            (   Source == ""
            ->  Result = ok
            ;   load_code(SessionId, Source, LoadError),
                (   LoadError == none
                ->  Result = ok
                ;   format(atom(Result), 'error(~w)', [LoadError])
                )
            )
        ),
        RollbackError,
        format(atom(Result), 'error(~w)', [RollbackError])
    ).

%% repl_harness_view(+SessionId, -Rows)
%% One row per predicate of the harness, for the live side view:
%%   "Name/Arity|ClauseCount|flag,flag"
%% Text rather than terms, so the host does not have to decode compound terms through
%% the bridge — the view needs identity (to diff) and a little detail, not structure.
repl_harness_view(SessionId, Rows) :-
    repl_user_predicates(SessionId, Preds),
    findall(Row,
        (   member(Name/Arity, Preds),
            repl_predicate_clause_count(SessionId, Name, Arity, Clauses),
            repl_harness_flags(SessionId, Name, Arity, Flags),
            (   Flags == []
            ->  FlagText = ""
            ;   atomic_list_concat(Flags, ',', FlagText)
            ),
            format(atom(Row), "~w/~w|~w|~w", [Name, Arity, Clauses, FlagText])
        ),
        Rows).

repl_predicate_clause_count(SessionId, Name, Arity, Count) :-
    functor(Head, Name, Arity),
    findall(x, clause(SessionId:Head, _), All),
    length(All, Count).

repl_harness_flags(SessionId, Name, Arity, Flags) :-
    findall(Flag,
        (   member(Flag, [entry, tool, doc, touched]),
            repl_harness_flag(SessionId, Name, Arity, Flag)
        ),
        Flags).

repl_harness_flag(_SessionId, agent_main, _Arity, entry).
repl_harness_flag(SessionId, Name, _Arity, tool) :-
    session_user_tools(SessionId, Name).
repl_harness_flag(SessionId, Name, Arity, doc) :-
    current_predicate(SessionId:doc/2),
    catch(SessionId:doc(Name/Arity, _), _, fail).
repl_harness_flag(SessionId, Name, Arity, touched) :-
    session_edit_touched(SessionId, Name/Arity).

%% repl_user_predicates(+SessionId, -Preds)
%% Sorted Name/Arity list of the harness's own predicates.
repl_user_predicates(SessionId, Preds) :-
    findall(Name/Arity,
        (   current_predicate(SessionId:Name/Arity),
            repl_user_predicate(SessionId, Name, Arity)
        ),
        Preds0),
    sort(Preds0, Preds).

%% repl_user_predicates_text(+SessionId, -Text)
%% Rendered "name/arity, name/arity" for change diffing on the host side.
repl_user_predicates_text(SessionId, Text) :-
    repl_user_predicates(SessionId, Preds),
    with_output_to(string(Text), print_pred_list(Preds)).

%% repl_predicate_arities(+SessionId, +Name, -Arities)
%% Sorted arities of a user predicate, e.g. agent_main -> [0,1,2]. Used by
%% `deepclause run` to pick the agent_main/N to call for the given arguments.
repl_predicate_arities(SessionId, Name, Arities) :-
    atom(Name),
    findall(Arity,
        (   current_predicate(SessionId:Name/Arity),
            repl_user_predicate(SessionId, Name, Arity)
        ),
        Arities0),
    sort(Arities0, Arities).

%% ============================================================
%% Nondeterminism probe (end_turn advisory)
%%
%% After a commit, the host may probe each changed pure predicate: if calling it
%% with fresh variables yields more than a threshold of solutions, the predicate
%% is probably missing a commit (`once/1` / `!`) and would flood an `execute`.
%% Side-effecting predicates are never run (see repl_impure_name/1), fact tables
%% are skipped, and the search is bounded by an inference limit.
%% ============================================================

%% Functors with side effects: model calls, host tools, file IO, DB mutation.
repl_impure_name(task). repl_impure_name(prompt). repl_impure_name(exec).
repl_impure_name(system). repl_impure_name(user). repl_impure_name(answer).
repl_impure_name(input). repl_impure_name(output). repl_impure_name(read_media).
repl_impure_name(attach_media). repl_impure_name(save_image). repl_impure_name(gen_image).
repl_impure_name(judge). repl_impure_name(choose). repl_impure_name(rate).
repl_impure_name(verify). repl_impure_name(probability).
repl_impure_name(write_file). repl_impure_name(append_file). repl_impure_name(delete_file).
repl_impure_name(make_directory_path). repl_impure_name(open). repl_impure_name(close).
repl_impure_name(assert). repl_impure_name(assertz). repl_impure_name(asserta).
repl_impure_name(retract). repl_impure_name(retractall).
repl_impure_name(consult). repl_impure_name(use_module). repl_impure_name(load_files).

%% repl_body_impure(+Goal) — true when the goal tree calls a side-effecting name.
repl_body_impure(Goal) :-
    callable(Goal),
    functor(Goal, Name, _),
    repl_impure_name(Name),
    !.
repl_body_impure((A, _)) :- repl_body_impure(A).
repl_body_impure((_, B)) :- repl_body_impure(B).
repl_body_impure((A ; B)) :- ( repl_body_impure(A) ; repl_body_impure(B) ).
repl_body_impure((A -> B)) :- ( repl_body_impure(A) ; repl_body_impure(B) ).
repl_body_impure(\+ Goal) :- repl_body_impure(Goal).
repl_body_impure(once(Goal)) :- repl_body_impure(Goal).
repl_body_impure(ignore(Goal)) :- repl_body_impure(Goal).
repl_body_impure(forall(Gen, Action)) :- ( repl_body_impure(Gen) ; repl_body_impure(Action) ).
repl_body_impure(catch(Goal, _, Recovery)) :- ( repl_body_impure(Goal) ; repl_body_impure(Recovery) ).
repl_body_impure(findall(_, Goal, _)) :- repl_body_impure(Goal).
repl_body_impure(findall(_, Goal, _, _)) :- repl_body_impure(Goal).
repl_body_impure(call_with_inference_limit(Goal, _, _)) :- repl_body_impure(Goal).
repl_body_impure(with_output_to(_, Goal)) :- repl_body_impure(Goal).
repl_body_impure(_:Goal) :- repl_body_impure(Goal).

repl_predicate_pure(SessionId, Name, Arity) :-
    functor(Head, Name, Arity),
    \+ ( clause(SessionId:Head, Body), repl_body_impure(Body) ).

%% ============================================================
%% Tool contract validation (end_turn advisory)
%% ============================================================
%%
%% A tool head is what the nested agent calls; the tool body is what runs. So a
%% well-formed tool is `tool(foo(A,B), "…") :- foo(A,B)` with foo/2 defined
%% separately. The declared head arrives structurally through `clause/2` (its
%% body is NOT executed), so a declared-but-undefined tool is caught here rather
%% than at call time deep inside a nested agent.
%%
%% Known limitation: a module-qualified skill delegate (e.g. `other:solve(A,B)`)
%% is not a session predicate and is reported; use `with_tools/2` for those.

%% repl_validate_tools(+SessionId, -Warnings)
repl_validate_tools(SessionId, Warnings) :-
    findall(W, tool_contract_violation(SessionId, W), Raw),
    list_to_set(Raw, Warnings).

%% repl_validate_docs(+SessionId, -Warnings)
%%
%% `doc(Name/Arity, Text)` is an annotation: it documents a predicate, it does not
%% create one. A `doc/2` whose predicate has no clauses therefore names a goal that
%% fails silently when called — the failure surfaces far away as an empty result,
%% and the name still appears in the manifest, so the listing looks healthy.
%% Reported at `end_turn`, where the author can still see what they just wrote.
repl_validate_docs(SessionId, Warnings) :-
    findall(W, doc_without_clauses(SessionId, W), Raw),
    list_to_set(Raw, Warnings).

doc_without_clauses(SessionId, Warning) :-
    % A harness with no `doc/2` at all would otherwise make this call an
    % undefined-procedure warning (`Unknown procedure: repl_x:doc/2`).
    current_predicate(SessionId:doc/2),
    SessionId:doc(Name/Arity, _),
    (   \+ current_predicate(SessionId:Name/Arity)
    ;   functor(Head, Name, Arity),
        \+ clause(SessionId:Head, _)
    ),
    !,
    format(string(Warning),
        'doc(~w/~w) documents a predicate with no clauses, so calling it fails silently and yields an empty result. Define ~w/~w, or remove the doc/2.',
        [Name, Arity, Name, Arity]).

tool_contract_violation(SessionId, Warning) :-
    clause(SessionId:tool(Head), Body),
    compound(Head),
    Head =.. [Name|HeadArgs],
    length(HeadArgs, Arity),
    tool_body_goal(Body, Call),
    Call =.. [Callee|Args],
    length(Args, CalleeArity),
    (   Callee == Name, CalleeArity == Arity, \+ repl_user_predicate(SessionId, Name, Arity)
    ->  format(string(Warning),
            'tool ~w/~w calls the tool itself, so a nested agent recurses forever. Define ~w/~w separately and delegate to it: tool(~w(A,B), "…") :- ~w(A,B).',
            [Name, Arity, Name, Name, Name, Name])
    ;   Callee == Name, CalleeArity \== Arity
    ->  format(string(Warning),
            'tool ~w/~w declares a different arity than its body calls (~w/~w). A tool head and its delegate must match, with the output argument last.',
            [Name, Arity, Callee, CalleeArity])
    ;   \+ ( SessionId:current_predicate(Callee/CalleeArity) )
    ->  format(string(Warning),
            'tool ~w/~w calls ~w/~w, which is not defined, so a nested agent gets "Tool not found". Define ~w/~w and delegate to it.',
            [Name, Arity, Callee, CalleeArity, Callee, CalleeArity])
    ).

%% tool_body_goal(+Body, -FirstCall): a tool body must reach a call, not be empty.
tool_body_goal(Body, Call) :-
    Body \== true,
    tool_first_goal(Body, Call),
    compound(Call),
    functor(Call, F, _),
    F \== (',').

%% tool_first_goal(+Body, -Call): the first call in a conjunction body.
tool_first_goal((A, _), Call) :- !, tool_first_goal(A, Call).
tool_first_goal(Call, Call).

%% True when the predicate has at least one clause with a real body (not a fact).
repl_predicate_has_rule(SessionId, Name, Arity) :-
    functor(Head, Name, Arity),
    clause(SessionId:Head, Body),
    Body \== true,
    !.

%% repl_probe_nondeterminism(+SessionId, +Name, +Arity, +Threshold, +Bound, -Result)
%% Result: one | many | timeout | data | unsafe | removed | error.
repl_probe_nondeterminism(SessionId, Name, Arity, Threshold, Bound, Result) :-
    (   \+ current_predicate(SessionId:Name/Arity)
    ->  Result = removed
    ;   \+ repl_predicate_has_rule(SessionId, Name, Arity)
    ->  Result = data
    ;   \+ repl_predicate_pure(SessionId, Name, Arity)
    ->  Result = unsafe
    ;   functor(Head, Name, Arity),
        N is Threshold + 1,
        catch(
            (   with_output_to(string(_),
                    call_with_inference_limit(call_nth(SessionId:Head, N), Bound, R)),
                (   R == false -> Result = one
                ;   R == inference_limit_exceeded -> Result = timeout
                ;   R == true -> Result = many
                ;   R == (!) -> Result = many   % success is returned as `!` in this build
                ;   Result = error
                )
            ),
            _,
            Result = error
        )
    ).

%% repl_history_listing(+SessionId)
%% Human-readable tail for `listing(history)`.
repl_history_listing(SessionId) :-
    (   current_predicate(SessionId:harness_head/4),
        SessionId:harness_head(Version, Timestamp, HeadSession, _Turn)
    ->  format("harness_head(~w, ~w, ~w)~n", [Version, Timestamp, HeadSession])
    ;   format("harness_head(none)~n")
    ),
    (   current_predicate(SessionId:harness_session/5)
    ->  forall(SessionId:harness_session(Sid, Started, Model, Backend, Dir),
            format("harness_session(~w, ~w, ~w, ~w, ~w)~n", [Sid, Started, Model, Backend, Dir]))
    ;   true
    ),
    (   current_predicate(SessionId:harness_change/5)
    ->  forall(SessionId:harness_change(V, Ts, Kind, Sid, Turn),
            format("harness_change(~w, ~w, ~w, ~w, ~w)~n", [V, Ts, Kind, Sid, Turn]))
    ;   true
    ).

%% repl_history_summary(+SessionId, -Text)
%% One-line history for the meta prompt manifest.
repl_history_summary(SessionId, Text) :-
    (   current_predicate(SessionId:harness_head/4),
        SessionId:harness_head(Version, Timestamp, HeadSession, _Turn)
    ->  true
    ;   Version = none, Timestamp = none, HeadSession = none
    ),
    (   current_predicate(SessionId:harness_session/5)
    ->  findall(s, SessionId:harness_session(_, _, _, _, _), Sessions0),
        length(Sessions0, Sessions)
    ;   Sessions = 0
    ),
    (   current_predicate(SessionId:harness_change/5)
    ->  findall(c, SessionId:harness_change(_, _, _, _, _), Changes0),
        length(Changes0, Changes)
    ;   Changes = 0
    ),
    format(atom(Text), 'version ~w (updated ~w by ~w), ~w session(s), ~w change(s)',
        [Version, Timestamp, HeadSession, Sessions, Changes]).

%% repl_meta_prompt(+SessionId, +Request, -PromptText)
%% Assembles the deterministic code-writing prompt: authoring contract, host
%% tool catalog, harness manifest, recent meta memory, the request, and the
%% output contract.
repl_meta_prompt(SessionId, Request, Prompt) :-
    (   session_meta_catalog(SessionId, Catalog) -> true ; Catalog = "" ),
    (   session_dml_reference(SessionId, Reference) -> true ; Reference = "" ),
    (   session_authoring_examples(SessionId, Authoring) -> true ; Authoring = "" ),
    (   session_harness_examples(SessionId, Examples) -> true ; Examples = "" ),
    (   session_prolog_snippets(SessionId, Snippets) -> true ; Snippets = "" ),
    repl_manifest(SessionId, Manifest),
    repl_meta_memory_text(SessionId, MemoryText),
    % The template is the bundled META_PROMPT.md, injected by the host via
    % repl_set_meta_template/2. The fallback keeps the prompt usable when no
    % template was set (e.g. a direct repl_meta_prompt/3 call).
    (   session_meta_template(SessionId, Template)
    ->  true
    ;   Template = "You are the DeepClause REPL harness author.~n~nHOST TOOL CATALOG~n{{CATALOG}}~n~nUSER REQUEST~n{{REQUEST}}~n"
    ),
    replace_all(Template, "{{CATALOG}}", Catalog, P1),
    replace_all(P1, "{{REFERENCE}}", Reference, P2),
    replace_all(P2, "{{AUTHORING}}", Authoring, P3),
    replace_all(P3, "{{EXAMPLES}}", Examples, P4),
    replace_all(P4, "{{SNIPPETS}}", Snippets, P5),
    replace_all(P5, "{{MANIFEST}}", Manifest, P6),
    replace_all(P6, "{{MEMORY}}", MemoryText, P7),
    replace_all(P7, "{{REQUEST}}", Request, Prompt).

%% replace_all(+Text, +From, +To, -Out)
%% Literal, all-occurrences substring replacement. Fills the meta-prompt
%% template's {{PLACEHOLDER}} sections.
replace_all(Text, From, To, Out) :-
    string_codes(Text, TextCodes),
    string_codes(From, FromCodes),
    string_codes(To, ToCodes),
    replace_all_codes(TextCodes, FromCodes, ToCodes, OutCodes),
    string_codes(Out, OutCodes).

replace_all_codes([], _, _, []) :- !.
replace_all_codes(Codes, From, To, Out) :-
    From \== [],
    (   append(From, Rest, Codes)
    ->  append(To, Tail, Out),
        replace_all_codes(Rest, From, To, Tail)
    ;   Codes = [C|Cs],
        Out = [C|Tail],
        replace_all_codes(Cs, From, To, Tail)
    ).

repl_meta_prompt(Request, Prompt) :-
    get_session_id(SessionId),
    repl_meta_prompt(SessionId, Request, Prompt).

%% repl_run_meta(+SessionId, +PromptText, -FrameId, -Error)
%% Creates the meta frame running the code-writing task.
repl_run_meta(SessionId, PromptText, FrameId, Error) :-
    catch(
        (   repl_initial_state(SessionId, meta, State),
            create_frame_raw(SessionId, meta, 0,
                deepclause_mi:repl_meta_solve(PromptText, SessionId, State),
                FrameId),
            Error = none
        ),
        CreateError,
        format(atom(Error), '~w', [CreateError])
    ).

repl_meta_solve(PromptText, SessionId, State) :-
    nb_setval(current_session_id, SessionId),
    catch(
        (   mi_call(
                task_named(PromptText, [DMLCode, Answer],
                    [typed_var{name:'DMLCode', type:string},
                     typed_var{name:'Answer', type:string}]),
                State, _StateOut),
            % The task's own memory (incl. the assembled prompt and tool results)
            % is ephemeral. The host records a clean user/assistant pair with
            % repl_record_meta/3 after the turn; that becomes the next turn's
            % conversation history (a normal messages array).
            engine_yield(meta_result(DMLCode, Answer))
        ),
        MetaError,
        (   format(atom(Msg), '~w', [MetaError]),
            engine_yield(error(Msg))
        )
    ).

%% repl_record_meta(+SessionId, +Request, +Summary)
%% Appends a short outcome note to meta memory (bounded).
repl_record_meta(SessionId, Request, Summary) :-
    (   session_meta_memory(SessionId, Memory)
    ->  true
    ;   Memory = []
    ),
    append(Memory,
        [message{role: user, content: Request},
         message{role: assistant, content: Summary}],
        Combined),
    % Keep every system message (compaction checkpoints) and the last 20
    % conversational messages; naive last_n/3 would evict old checkpoints.
    findall(M, (member(M, Combined), repl_system_message(M)), SystemMessages),
    findall(M, (member(M, Combined), \+ repl_system_message(M)), Conversational),
    last_n(Conversational, 20, Recent),
    append(SystemMessages, Recent, Bounded),
    retractall(session_meta_memory(SessionId, _)),
    assertz(session_meta_memory(SessionId, Bounded)).

%% repl_system_message(+Message)
repl_system_message(message{role: system, content: _}).

%% repl_get_memory(+SessionId, +Slot, -Messages)
%% Read a live memory slot as the message list stored in the session.
repl_get_memory(SessionId, Slot, Messages) :-
    (   Slot == meta
    ->  (   session_meta_memory(SessionId, Memory)
        ->  true
        ;   Memory = []
        )
    ;   (   session_repl_memory(SessionId, Memory)
        ->  true
        ;   Memory = []
        )
    ),
    Messages = Memory.

%% repl_set_memory(+SessionId, +Slot, +Messages)
%% Replace a live memory slot (used by host-side TypeScript compaction).
repl_set_memory(SessionId, Slot, Messages) :-
    (   Slot == meta
    ->  retractall(session_meta_memory(SessionId, _)),
        assertz(session_meta_memory(SessionId, Messages))
    ;   retractall(session_repl_memory(SessionId, _)),
        assertz(session_repl_memory(SessionId, Messages))
    ).

%% ---- Edit transactions ----

%% repl_snapshot_save(+SessionId)
%% Snapshots the living module, tool bookkeeping, directives, and harness
%% memory so a failed prompt leaves the harness untouched.
repl_snapshot_save(SessionId) :-
    retractall(repl_snapshot(SessionId, _)),
    findall(Clause,
        (   current_predicate(SessionId:Name/Arity),
            repl_user_predicate(SessionId, Name, Arity),
            functor(Head, Name, Arity),
            clause(SessionId:Head, Body),
            (   Body == true
            ->  Clause = Head
            ;   Clause = (Head :- Body)
            )
        ),
        Clauses),
    findall(Name, session_user_tools(SessionId, Name), Tools),
    findall(Name-Schema, session_user_tool_schema(SessionId, Name, Schema), Schemas),
    (   current_predicate(SessionId:tool_source/2)
    ->  findall(Name-Source, SessionId:tool_source(Name, Source), ToolSources)
    ;   ToolSources = []
    ),
    findall(Directive, session_directive(SessionId, Directive), Directives),
    (   session_repl_memory(SessionId, Memory) -> true ; Memory = [] ),
    assertz(repl_snapshot(SessionId, snapshot{
        clauses: Clauses,
        tools: Tools,
        schemas: Schemas,
        tool_sources: ToolSources,
        directives: Directives,
        memory: Memory
    })).

%% repl_snapshot_restore(+SessionId)
repl_snapshot_restore(SessionId) :-
    (   repl_snapshot(SessionId, Snapshot)
    ->  retract(repl_snapshot(SessionId, Snapshot)),
        findall(Name/Arity,
            (   current_predicate(SessionId:Name/Arity),
                repl_user_predicate(SessionId, Name, Arity)
            ),
            Preds),
        forall(member(AbName/AbArity, Preds),
            catch(abolish(SessionId:AbName/AbArity), _, true)),
        forall(member(Clause, Snapshot.clauses), assertz(SessionId:Clause)),
        retractall(session_user_tools(SessionId, _)),
        forall(member(Tool, Snapshot.tools), assertz(session_user_tools(SessionId, Tool))),
        retractall(session_user_tool_schema(SessionId, _, _)),
        forall(member(SchemaName-Schema, Snapshot.schemas),
            assertz(session_user_tool_schema(SessionId, SchemaName, Schema))),
        (   current_predicate(SessionId:tool_source/2)
        ->  retractall(SessionId:tool_source(_, _))
        ;   true
        ),
        forall(member(SourceName-Source, Snapshot.tool_sources),
            assertz(SessionId:tool_source(SourceName, Source))),
        retractall(session_directive(SessionId, _)),
        forall(member(Directive, Snapshot.directives),
            assertz(session_directive(SessionId, Directive))),
        retractall(session_repl_memory(SessionId, _)),
        assertz(session_repl_memory(SessionId, Snapshot.memory))
    ;   true
    ).

%% repl_parse_source(+SessionId, +Source, -Preds, -Error)
%% Validates generated DML and collects the predicates it defines.
repl_parse_source(SessionId, Source, Preds, Error) :-
    catch(
        (   open_string(Source, Stream),
            repl_collect_source_predicates(Stream, SessionId, Preds0),
            close(Stream),
            (   member(Name0/_, Preds0),
                repl_protected_name(Name0)
            ->  format(atom(Error), 'generated code defines reserved predicate ~w', [Name0])
            ;   sort(Preds0, Preds),
                Error = none
            )
        ),
        ParseError,
        format(atom(Error), '~w', [ParseError])
    ).

%% repl_collect_source_predicates(+Stream, +SessionId, -Preds)
%% Reads with the session's operator table and executes `:- use_module(...)`
%% directives as it goes, exactly like the load path. This is what lets library
%% operators (CLP(FD)'s #=/ins, etc.) be used in committed source: without it,
%% validation parsed the source with an empty operator table and rejected code
%% that loads fine.
repl_collect_source_predicates(Stream, SessionId, Preds) :-
    read_term(Stream, Term, [module(SessionId)]),
    (   Term == end_of_file
    ->  Preds = []
    ;   (   Term = (:- Directive)
        ->  catch(process_directive(Directive, SessionId), _, true),
            Preds = Rest
        ;   Term = repl_delete(_)
        ->  Preds = Rest
        ;   Term = (Head :- _)
        ->  functor(Head, Name, Arity),
            Preds = [Name/Arity|Rest]
        ;   functor(Term, Name, Arity),
            Preds = [Name/Arity|Rest]
        ),
        repl_collect_source_predicates(Stream, SessionId, Rest)
    ).

%% repl_commit(+SessionId, +Source, -Result)
%% Transactional commit: validate, replace-by-predicate, then load the returned
%% source. Result = ok | 'error(Message)'. Never throws; failures are reported
%% through Result so the caller can decide whether to repair.
repl_commit(SessionId, Source, Result) :-
    catch(
        repl_commit_unsafe(SessionId, Source, Result),
        CommitError,
        format(atom(Result), 'error(~w)', [CommitError])
    ).

repl_commit_unsafe(SessionId, Source, Result) :-
    guard_not_frozen(SessionId),
    repl_parse_source(SessionId, Source, Preds, ParseError),
    repl_commit_from_parse(SessionId, Source, Preds, ParseError, Result).

repl_commit_from_parse(_SessionId, _Source, _Preds, ParseError, Result) :-
    ParseError \== none,
    !,
    commit_error(ParseError, Result).
repl_commit_from_parse(SessionId, Source, Preds, none, Result) :-
    forall(member(Name/Arity, Preds),
        catch(abolish(SessionId:Name/Arity), _, true)),
    load_code(SessionId, Source, LoadError),
    (   LoadError == none
    ->  retractall(session_last_commit(SessionId, _)),
        assertz(session_last_commit(SessionId, Preds))
    ;   true
    ),
    repl_commit_from_load(LoadError, Result).

%% repl_last_commit_summary(+SessionId, -Text)
%% Rendered "name/arity, name/arity" of the predicates the last commit replaced.
repl_last_commit_summary(SessionId, Text) :-
    (   session_last_commit(SessionId, Preds), Preds \= []
    ->  with_output_to(string(Text), print_pred_list(Preds))
    ;   Text = ""
    ).

print_pred_list([]).
print_pred_list([Name/Arity]) :-
    !,
    format("~w/~w", [Name, Arity]).
print_pred_list([Name/Arity|Rest]) :-
    format("~w/~w, ", [Name, Arity]),
    print_pred_list(Rest).

repl_commit_from_load(LoadError, Result) :-
    LoadError \== none,
    !,
    commit_error(LoadError, Result).
repl_commit_from_load(none, ok).

commit_error(Message, Result) :-
    format(atom(Result), 'error(~w)', [Message]).

%% repl_execute_frame(+SessionId, +GoalText, -FrameId, -Error)
%% Creates a child frame for the meta `execute` tool.
repl_execute_frame(SessionId, GoalText, FrameId, Error) :-
    catch(
        (   read_repl_goal(SessionId, GoalText, Goal, Names),
            (   repl_reserved(Goal)
            ->  throw(error(permission_error(execute, reserved_predicate, Goal),
                        context(repl_execute_frame/4, 'cannot execute a reserved predicate')))
            ;   true
            ),
            repl_initial_state(SessionId, nested, State),
            create_frame_raw(SessionId, nested, 1,
                deepclause_mi:repl_solve(Goal, Names, SessionId, State),
                FrameId),
            Error = none
        ),
        ExecuteError,
        format(atom(Error), '~w', [ExecuteError])
    ).

%% determine_agent_goal(+SessionId, +Args, -Goal)
%% Args is a list of positional arguments - passed directly to agent_main
%% Prefers arity that matches the number of arguments provided
determine_agent_goal(SessionId, [], SessionId:agent_main) :-
    current_predicate(SessionId:agent_main/0), !.
determine_agent_goal(SessionId, [Arg1], SessionId:agent_main(Arg1)) :-
    current_predicate(SessionId:agent_main/1), !.
determine_agent_goal(SessionId, [Arg1, Arg2], SessionId:agent_main(Arg1, Arg2)) :-
    current_predicate(SessionId:agent_main/2), !.
determine_agent_goal(SessionId, [Arg1, Arg2, Arg3], SessionId:agent_main(Arg1, Arg2, Arg3)) :-
    current_predicate(SessionId:agent_main/3), !.
% Fallback: if no exact arity match, try best fit
determine_agent_goal(SessionId, Args, Goal) :-
    current_predicate(SessionId:agent_main/1), !,
    (   Args = [Arg1|_]
    ->  Goal = SessionId:agent_main(Arg1)
    ;   Goal = SessionId:agent_main(_)
    ).
determine_agent_goal(SessionId, Args, Goal) :-
    current_predicate(SessionId:agent_main/2), !,
    (   Args = [Arg1, Arg2|_]
    ->  Goal = SessionId:agent_main(Arg1, Arg2)
    ;   Args = [Arg1|_]
    ->  Goal = SessionId:agent_main(Arg1, _)
    ;   Goal = SessionId:agent_main(_, _)
    ).
determine_agent_goal(SessionId, Args, Goal) :-
    current_predicate(SessionId:agent_main/3), !,
    (   Args = [Arg1, Arg2, Arg3|_]
    ->  Goal = SessionId:agent_main(Arg1, Arg2, Arg3)
    ;   Args = [Arg1, Arg2|_]
    ->  Goal = SessionId:agent_main(Arg1, Arg2, _)
    ;   Args = [Arg1|_]
    ->  Goal = SessionId:agent_main(Arg1, _, _)
    ;   Goal = SessionId:agent_main(_, _, _)
    ).
determine_agent_goal(SessionId, _Args, SessionId:agent_main).

%% step_engine(+SessionId, -Status, -Content, -Payload)
%% Legacy single-engine entry point: resolves the session's default frame and
%% delegates to step_frame/4. Kept as a thin wrapper so existing callers are
%% unaffected by the frame refactor.
step_engine(SessionId, Status, Content, Payload) :-
    (   session_default_frame(SessionId, FrameId)
    ->  step_frame(FrameId, Status, Content, Payload)
    ;   Status = error,
        Content = 'No engine found for session',
        Payload = none
    ).

%% process_engine_result(+Result, -Status, -Content, -Payload)
process_engine_result(output(Text), output, Text, none) :- !.
process_engine_result(media(Payload), media, '', Payload) :- is_dict(Payload), !.
process_engine_result(log(Text), log, Text, none) :- !.
process_engine_result(answer(Text), answer, Text, none) :- !.
%% Current 8-arg format (with model spec from with_model/2)
process_engine_result(request_agent_loop(Desc, Vars, Tools, Memory, ToolScope, ReasoningEffort, RecipeContext, ModelSpec), request_agent_loop, '',
    payload{taskDescription: Desc, outputVars: Vars, userTools: Tools, memory: Memory, toolScope: ToolScope, reasoningEffort: ReasoningEffort, recipeContext: RecipeContext, modelSpec: ModelSpec}) :- !.
%% Prompt-family 6-arg format (tool scope + model spec; no reasoning/recipe)
process_engine_result(request_agent_loop(Desc, Vars, Tools, Memory, ToolScope, ModelSpec), request_agent_loop, '',
    payload{taskDescription: Desc, outputVars: Vars, userTools: Tools, memory: Memory, toolScope: ToolScope, reasoningEffort: none, recipeContext: "", modelSpec: ModelSpec}) :- !.
%% Note: Memory and tool scope are now passed in the payload for the agent loop
process_engine_result(request_agent_loop(Desc, Vars, Tools, Memory, ToolScope, ReasoningEffort, RecipeContext), request_agent_loop, '',
    payload{taskDescription: Desc, outputVars: Vars, userTools: Tools, memory: Memory, toolScope: ToolScope, reasoningEffort: ReasoningEffort, recipeContext: RecipeContext}) :- !.
%% Legacy 5-arg format (no reasoning/recipe)
process_engine_result(request_agent_loop(Desc, Vars, Tools, Memory, ToolScope), request_agent_loop, '', 
    payload{taskDescription: Desc, outputVars: Vars, userTools: Tools, memory: Memory, toolScope: ToolScope, reasoningEffort: none, recipeContext: ""}) :- !.
%% Legacy 4-arg format (no tool scope)
process_engine_result(request_agent_loop(Desc, Vars, Tools, Memory), request_agent_loop, '', 
    payload{taskDescription: Desc, outputVars: Vars, userTools: Tools, memory: Memory, toolScope: none, reasoningEffort: none, recipeContext: ""}) :- !.
process_engine_result(request_exec(Tool, Args), request_exec, '',
    payload{toolName: Tool, args: Args}) :- !.
%% Process primitives (spawn/send/receive/...) are yielded to the host, which
%% owns identity, mailboxes, links, monitors, and naming.
process_engine_result(request_proc(Request), request_proc, '', Payload) :-
    proc_request_payload(Request, Payload).

%% proc_request_payload(+Request, -Payload)
proc_request_payload(self, payload{op: self}) :- !.
proc_request_payload(spawn(Goal), payload{op: spawn, goal: Goal}) :- !.
proc_request_payload(spawn(Goal, Options), payload{op: spawn, goal: Goal, options: Options}) :- !.
proc_request_payload(send(To, Message), payload{op: send, to: To, message: Message}) :- !.
proc_request_payload(receive(Timeout), payload{op: receive, timeout: Timeout}) :- !.
proc_request_payload(requeue(Message), payload{op: requeue, message: Message}) :- !.
proc_request_payload(wait(Pid), payload{op: wait, pid: Pid}) :- !.
proc_request_payload(exit(Reason), payload{op: exit, reason: Reason}) :- !.
proc_request_payload(exit(Pid, Reason), payload{op: exit, pid: Pid, reason: Reason}) :- !.
proc_request_payload(process_info(Pid), payload{op: process_info, pid: Pid}) :- !.
proc_request_payload(register(Name, Pid), payload{op: register, name: Name, pid: Pid}) :- !.
proc_request_payload(whereis(Name), payload{op: whereis, name: Name}) :- !.
proc_request_payload(sleep(Milliseconds), payload{op: sleep, ms: Milliseconds}) :- !.
process_engine_result(request_sample_token(Prompt, Allowed, ModelSpec), request_sample_token, '',
    payload{prompt: Prompt, allowedTokens: Allowed, modelSpec: ModelSpec}) :- !.
process_engine_result(request_sample_token(Prompt, Allowed), request_sample_token, '',
    payload{prompt: Prompt, allowedTokens: Allowed, modelSpec: none}) :- !.
process_engine_result(request_llm(Messages, ModelSpec), request_llm, '',
    payload{messages: Messages, modelSpec: ModelSpec}) :- !.
process_engine_result(request_llm(Messages), request_llm, '',
    payload{messages: Messages, modelSpec: none}) :- !.
process_engine_result(request_judgment(Payload), request_judgment, '', Payload) :- !.
%% Tool result from inline tool execution
process_engine_result(tool_result(Result), tool_result, '',
    payload{result: Result}) :- !.
%% Goal-mode solution: bindings is a list of Name-RenderedText pairs.
process_engine_result(solution(Bindings, Deterministic), solution, '',
    payload{bindings: Bindings, deterministic: Deterministic}) :- !.
%% Prompt-mode meta task result: generated DML source and an optional direct
%% answer for simple questions.
process_engine_result(meta_result(Code, Answer), meta_result, '',
    payload{code: Code, answer: Answer}) :- !.
process_engine_result(wait_input(Prompt), wait_input, Prompt, none) :- !.
process_engine_result(error(Msg), error, Msg, none) :- !.
process_engine_result(Other, error, Msg, none) :-
    format(atom(Msg), 'Unknown engine result: ~w', [Other]).

%% destroy_engine(+SessionId)
destroy_engine(SessionId) :-
    % Destroy every frame belonging to this session (the default query frame and
    % any nested frames still alive). end_frame/1 also clears the default pointer.
    (   findall(FrameId, session_frame(FrameId, SessionId, _, _), FrameIds)
    ->  forall(member(FrameId, FrameIds), end_frame(FrameId))
    ;   true
    ),
    (   session_engine(SessionId, Engine)
    ->  catch(engine_destroy(Engine), _, true)
    ;   true
    ),
    retractall(session_default_frame(SessionId, _)),
    retractall(session_engine(SessionId, _)),
    retractall(session_params(SessionId, _)),
    retractall(session_user_tools(SessionId, _)),
    retractall(session_user_tool_schema(SessionId, _, _)),
    retractall(session_pending_input(SessionId, _)),
    retractall(session_input_cancelled(SessionId)),
    retractall(session_agent_result(SessionId, _)),
    retractall(session_exec_result(SessionId, _)),
    retractall(session_sample_token_result(SessionId, _)),
    retractall(session_llm_result(SessionId, _)),
    retractall(session_judgment_result(SessionId, _)),
    retractall(session_trace_log(SessionId, _)),
    retractall(session_trace_enabled(SessionId, _)),
    (   current_module(SessionId)
    ->  catch(
            (   findall(Head, 
                    (current_predicate(SessionId:Name/Arity), 
                     functor(Head, Name, Arity), 
                     clause(SessionId:Head, _)),
                    Heads),
                forall(member(H, Heads), retractall(SessionId:H))
            ),
            _,
            true
        )
    ;   true
    ).

%% ============================================================
%% Result Posting (from JavaScript)
%% ============================================================

%% post_agent_result(+SessionId, +Success, +Variables, +Messages)
%% Legacy form without a full memory replacement snapshot.
post_agent_result(SessionId, Success, Variables, Messages) :-
    post_agent_result(SessionId, Success, Variables, Messages, none).

%% post_agent_result(+SessionId, +Success, +Variables, +Messages, +FullMemory)
%% Messages is a list of message{role: Role, content: Content} dicts.
%% FullMemory optionally replaces the entire state memory, including compacted system summaries.
post_agent_result(SessionId, Success, Variables, Messages, FullMemory) :-
    retractall(session_agent_result(SessionId, _)),
    assertz(session_agent_result(SessionId, result{success: Success, variables: Variables, messages: Messages, full_memory: FullMemory})),
    post_signal_to_engine(SessionId, agent_done).

%% post_exec_result(+SessionId, +Status, +Result)
post_exec_result(SessionId, Status, Result) :-
    retractall(session_exec_result(SessionId, _)),
    assertz(session_exec_result(SessionId, result{status: Status, result: Result})),
    post_signal_to_engine(SessionId, exec_done).

%% post_proc_result(+SessionId, +Key, +Value)
%% Reply to a process primitive (spawn/send/receive/...). Key names the result.
post_proc_result(SessionId, Key, Value) :-
    retractall(session_proc_result(SessionId, Key, _)),
    assertz(session_proc_result(SessionId, Key, Value)),
    post_signal_to_engine(SessionId, proc_done).

%% post_sample_token_result(+SessionId, +Status, +TokenOrError)
post_sample_token_result(SessionId, Status, TokenOrError) :-
    retractall(session_sample_token_result(SessionId, _)),
    assertz(session_sample_token_result(SessionId, result{status: Status, token: TokenOrError})),
    post_signal_to_engine(SessionId, sample_token_done).

%% post_judgment_result(+SessionId, +Status, +ValuesOrError)
%% On success Values is the list of answer values in question order.
post_judgment_result(SessionId, success, Values) :-
    retractall(session_judgment_result(SessionId, _)),
    assertz(session_judgment_result(SessionId, result{status: success, values: Values})),
    post_signal_to_engine(SessionId, judgment_done).
post_judgment_result(SessionId, failure, Error) :-
    retractall(session_judgment_result(SessionId, _)),
    assertz(session_judgment_result(SessionId, result{status: failure, error: Error})),
    post_signal_to_engine(SessionId, judgment_done).

%% post_llm_result(+SessionId, +Status, +TextOrError)
post_llm_result(SessionId, Status, TextOrError) :-
    retractall(session_llm_result(SessionId, _)),
    assertz(session_llm_result(SessionId, result{status: Status, text: TextOrError})),
    post_signal_to_engine(SessionId, llm_done).

%% provide_input(+SessionId, +Input)
provide_input(SessionId, Input) :-
    assertz(session_pending_input(SessionId, Input)),
    post_signal_to_engine(SessionId, input_provided).

%% provide_input_cancelled(+SessionId)
%% Non-interactive runs have no human. Signal the waiting input/2 to fail
%% instead of blocking.
provide_input_cancelled(SessionId) :-
    assertz(session_input_cancelled(SessionId)),
    post_signal_to_engine(SessionId, input_provided).

%% post_signal_to_engine(+SessionId, +Signal)
%% Posts a signal via dynamic predicate (most reliable for WASM engines)
%% Falls back to engine_post only if dynamic assert fails
post_signal_to_engine(SessionId, Signal) :-
    % Always use dynamic predicate - more reliable than engine_post in WASM
    assertz(session_pending_signal(SessionId, Signal)).

%% ============================================================
%% Meta-Interpreter Core - STATE THREADED VERSION
%% ============================================================

%% mi(+Goal, +StateIn, +SessionId)
%% Main meta-interpreter entry point
%% State = state{memory: [...], params: {...}}
mi(Goal, StateIn, SessionId) :-
    nb_setval(current_session_id, SessionId),
    catch(
        mi_call(Goal, StateIn, _StateOut),
        Error,
        (   Error == '$answer_commit'
        ->  true  % answer/1 threw to commit - success, no backtracking
        ;   Error == gas_exhausted
        ->  engine_yield(error("Gas exhausted: execution exceeded iteration limit")),
            fail
        ;   format(atom(ErrMsg), 'Runtime error: ~w', [Error]),
            engine_yield(error(ErrMsg)),
            fail
        )
    ),
    !.  % Cut to prevent backtracking after goal completes

%% ============================================================
%% State Helpers
%% ============================================================

%% add_memory(+StateIn, +Role, +Content, -StateOut)
add_memory(StateIn, Role, Content, StateOut) :-
    Message = message{role: Role, content: Content},
    OldMemory = StateIn.memory,
    append(OldMemory, [Message], NewMemory),
    StateOut = StateIn.put(memory, NewMemory).

%% get_memory(+State, -Memory)
get_memory(State, Memory) :-
    Memory = State.memory.

%% get_params(+State, -Params)
get_params(State, Params) :-
    Params = State.params.

%% get_context_stack(+State, -Stack)
get_context_stack(State, Stack) :-
    (   get_dict(context_stack, State, Stack)
    ->  true
    ;   Stack = []
    ).

%% set_context_stack(+StateIn, +Stack, -StateOut)
set_context_stack(StateIn, Stack, StateOut) :-
    StateOut = StateIn.put(context_stack, Stack).

%% set_memory(+StateIn, +Memory, -StateOut)
set_memory(StateIn, Memory, StateOut) :-
    StateOut = StateIn.put(memory, Memory).

%% is_system_memory_message(+Message)
%% True if the memory message has role=system
is_system_memory_message(Message) :-
    is_dict(Message),
    get_dict(role, Message, system).

%% ============================================================
%% Tool Scope Helpers
%% ============================================================
%% Tool scoping allows manual control over which tools are available
%% in nested tasks. The scope is stored in state and passed through
%% the request_agent_loop payload.

%% get_tool_scope(+State, -Scope)
%% Returns the current tool scope, or 'none' if not set
get_tool_scope(State, Scope) :-
    (   get_dict(tool_scope, State, Scope)
    ->  true
    ;   Scope = none
    ).

%% set_tool_scope(+StateIn, +Scope, -StateOut)
set_tool_scope(StateIn, Scope, StateOut) :-
    StateOut = StateIn.put(tool_scope, Scope).

%% clear_tool_scope(+StateIn, -StateOut)
clear_tool_scope(StateIn, StateOut) :-
    (   get_dict(tool_scope, StateIn, _)
    ->  del_dict(tool_scope, StateIn, _, StateOut)
    ;   StateOut = StateIn
    ).

%% ============================================================
%% Reasoning Effort Helpers
%% ============================================================

%% set_reasoning_effort_state(+StateIn, +Effort, -StateOut)
set_reasoning_effort_state(StateIn, Effort, StateOut) :-
    StateOut = StateIn.put(reasoning_effort, Effort).

%% clear_reasoning_effort_state(+StateIn, -StateOut)
clear_reasoning_effort_state(StateIn, StateOut) :-
    (   get_dict(reasoning_effort, StateIn, _)
    ->  del_dict(reasoning_effort, StateIn, _, StateOut)
    ;   StateOut = StateIn
    ).

%% get_reasoning_effort(+State, -Effort)
get_reasoning_effort(State, Effort) :-
    (   get_dict(reasoning_effort, State, Effort)
    ->  true
    ;   Effort = none
    ).

%% set_model_state(+StateIn, +Spec, -StateOut)
set_model_state(StateIn, Spec, StateOut) :-
    StateOut = StateIn.put(model_spec, Spec).

%% clear_model_state(+StateIn, -StateOut)
clear_model_state(StateIn, StateOut) :-
    (   get_dict(model_spec, StateIn, _)
    ->  del_dict(model_spec, StateIn, _, StateOut)
    ;   StateOut = StateIn
    ).

%% get_model_spec(+State, -Spec)
%% `none` means "no override" (use the session model).
get_model_spec(State, Spec) :-
    (   get_dict(model_spec, State, Spec)
    ->  true
    ;   Spec = none
    ).

%% ============================================================
%% Recipe Context Helpers
%% ============================================================

%% load_recipe_content(+Query, +StateIn, -StateOut, -Content)
load_recipe_content(Query, StateIn, StateOut, RecipeContent) :-
    mi_call(exec(consult_recipes(query: Query, max_results: 1), Result), StateIn, StateOut),
    get_dict(matches, Result, Matches),
    (   Matches = [Match|_]
    ->  get_dict(content, Match, RecipeContent)
    ;   RecipeContent = ""
    ).

%% set_recipe_context(+StateIn, +Content, -StateOut)
set_recipe_context(StateIn, "", StateIn) :- !.
set_recipe_context(StateIn, Content, StateOut) :-
    StateOut = StateIn.put(recipe_context, Content).

%% clear_recipe_context(+StateIn, -StateOut)
clear_recipe_context(StateIn, StateOut) :-
    (   get_dict(recipe_context, StateIn, _)
    ->  del_dict(recipe_context, StateIn, _, StateOut)
    ;   StateOut = StateIn
    ).

%% get_recipe_context(+State, -Context)
get_recipe_context(State, Context) :-
    (   get_dict(recipe_context, State, Context)
    ->  true
    ;   Context = ""
    ).

%% ============================================================
%% Trace Helpers
%% ============================================================

%% get_depth(+State, -Depth)
get_depth(State, Depth) :-
    (   get_dict(depth, State, Depth)
    ->  true
    ;   Depth = 0
    ).

%% set_depth(+StateIn, +Depth, -StateOut)
set_depth(StateIn, Depth, StateOut) :-
    StateOut = StateIn.put(depth, Depth).

%% add_trace_entry(+SessionId, +Type, +Predicate, +Args, +Depth)
%% Add a trace entry if tracing is enabled
add_trace_entry(SessionId, Type, Predicate, Args, Depth) :-
    (   session_trace_enabled(SessionId, true)
    ->  get_time(Now),
        Timestamp is round(Now * 1000),  % Convert to milliseconds
        Entry = trace{timestamp: Timestamp, type: Type, predicate: Predicate, args: Args, depth: Depth},
        session_trace_log(SessionId, OldLog),
        retract(session_trace_log(SessionId, OldLog)),
        append(OldLog, [Entry], NewLog),
        assertz(session_trace_log(SessionId, NewLog))
    ;   true
    ).

%% add_trace_with_result(+SessionId, +Type, +Predicate, +Args, +Result, +Depth)
%% Add a trace entry with a result field
add_trace_with_result(SessionId, Type, Predicate, Args, Result, Depth) :-
    (   session_trace_enabled(SessionId, true)
    ->  get_time(Now),
        Timestamp is round(Now * 1000),
        Entry = trace{timestamp: Timestamp, type: Type, predicate: Predicate, args: Args, result: Result, depth: Depth},
        session_trace_log(SessionId, OldLog),
        retract(session_trace_log(SessionId, OldLog)),
        append(OldLog, [Entry], NewLog),
        assertz(session_trace_log(SessionId, NewLog))
    ;   true
    ).

%% ============================================================
%% Agent Signal Loop - SIMPLIFIED
%% ============================================================
%%
%% With isolated tool execution, the agent signal loop is much simpler.
%% It only needs to wait for agent_done from the host after a task() completes.
%% Tool calls are handled separately via execute_tool_isolated/4.

%% handle_agent_signals(+SessionId, +StateIn, -StateOut)
%% Wait for agent_done signal from host after task() request.
%% State passes through unchanged (tool results don't modify task state).
handle_agent_signals(SessionId, StateIn, StateOut) :-
    get_next_signal(SessionId, Signal),
    handle_signal(SessionId, Signal, StateIn, StateOut).

%% get_next_signal(+SessionId, -Signal)
%% Retrieves the next signal, preferring dynamic predicate over engine_fetch.
get_next_signal(SessionId, Signal) :-
    (   retract(session_pending_signal(SessionId, Signal))
    ->  true
    ;   catch(
            engine_fetch(Signal),
            _FetchError,
            (   retract(session_pending_signal(SessionId, Signal))
            ->  true
            ;   Signal = error_signal(no_signal_available)
            )
        )
    ).

%% handle_signal(+SessionId, +Signal, +StateIn, -StateOut)
handle_signal(_SessionId, agent_done, State, State) :- !.

handle_signal(_SessionId, error_signal(Error), State, State) :-
    !,
    format(atom(ErrMsg), 'Engine fetch error: ~w', [Error]),
    engine_yield(error(ErrMsg)).

handle_signal(SessionId, exec_done, StateIn, StateOut) :-
    % Stale signal - ignore and continue waiting
    !,
    handle_agent_signals(SessionId, StateIn, StateOut).

handle_signal(SessionId, llm_done, StateIn, StateOut) :-
    % Stale signal - ignore and continue waiting
    !,
    handle_agent_signals(SessionId, StateIn, StateOut).

handle_signal(SessionId, _Unknown, StateIn, StateOut) :-
    % Unknown signal - ignore and continue waiting
    !,
    handle_agent_signals(SessionId, StateIn, StateOut).

%% call_tool_inline/6 is DEPRECATED - kept for backwards compatibility only
%% New code should use execute_tool_isolated/4 instead.
%% call_tool_inline(+SessionId, +ToolName, +Args, +StateIn, -StateOut, -Result)
%% Executes a DML-defined tool inline in the current engine.
%% The tool body runs through the meta-interpreter, sharing state.
%% Args is a list of input argument values from the tool call.
%% The tool may have additional output arguments which become unbound variables.
call_tool_inline(SessionId, ToolName, Args, StateIn, StateOut, Result) :-
    % Convert tool name to atom
    atom_string(ToolNameAtom, ToolName),
    % Find the tool clause to determine its actual arity
    % Tools are stored as: SessionId:(tool(Head) :- Body)
    (   clause(SessionId:tool(ToolPattern), Body),
        ToolPattern =.. [ToolNameAtom|PatternArgs]
    ->  % Found the tool - create head args with inputs filled in
        length(PatternArgs, TotalArity),
        length(Args, InputArity),
        % Create the head with input args + unbound output variables
        length(HeadArgs, TotalArity),
        % Fill in the input args
        fill_args(HeadArgs, Args, InputArity),
        ToolHead =.. [ToolNameAtom|HeadArgs],
        % Unify with the pattern to bind variables in Body
        ToolHead = ToolPattern,
        % Execute the tool body through MI
        % Wrap in catch to handle answer() commits - answer() yields and then throws
        catch(
            (   mi_call(Body, StateIn, StateOut)
            ->  % Collect result from result/1 facts or output args
                % Use catch to handle case where result/2 doesn't exist
                (   catch(SessionId:result(ToolNameAtom, R), _, fail)
                ->  Result = R,
                    catch(retract(SessionId:result(ToolNameAtom, R)), _, true)
                ;   % No explicit result - return the last output arg if bound
                    (   TotalArity > InputArity,
                        last(HeadArgs, LastArg),
                        nonvar(LastArg)
                    ->  Result = LastArg
                    ;   Result = true
                    )
                )
            ;   % Tool body failed
                StateOut = StateIn,
                Result = false
            ),
            '$answer_commit',
            % answer() was called - it already yielded the answer text
            % Just set success result and state
            (StateOut = StateIn, Result = answered)
        )
    ;   % No DML body - should not happen if host checked
        StateOut = StateIn,
        format(atom(Result), 'Tool ~w not found', [ToolName])
    ).

%% ============================================================
%% ISOLATED Tool Execution (Engine-based for exec() support)
%% ============================================================
%%
%% Tools run in their own engine to support exec() calls.
%% TypeScript creates the engine, steps it, handles request_exec,
%% and collects the result when the engine finishes.
%% exec() results are posted via the SESSION's exec_result mechanism.

%% Tool engine dynamic predicates
:- dynamic tool_engine/2.        % tool_engine(ToolEngineId, Engine)

%% create_tool_engine(+SessionId, +ToolName, +Args, -ToolEngineId)
%% Creates an engine to execute a DML tool. Returns a unique engine ID.
create_tool_engine(SessionId, ToolName, Args, ToolEngineId) :-
    atom_string(ToolNameAtom, ToolName),
    % Generate unique tool engine ID
    gensym(tool_engine_, ToolEngineId),
    (   clause(SessionId:tool(ToolPattern), Body),
        ToolPattern =.. [ToolNameAtom|PatternArgs]
    ->  length(PatternArgs, TotalArity),
        length(Args, InputArity),
        length(HeadArgs, TotalArity),
        fill_args(HeadArgs, Args, InputArity),
        ToolHead =.. [ToolNameAtom|HeadArgs],
        ToolHead = ToolPattern,
        (session_params(SessionId, Params) -> true ; Params = params{}),
        InitialState = state{memory: [], params: Params, context_stack: [], depth: 0},
        % Create goal that will yield the result
        Goal = (
            nb_setval(current_session_id, SessionId),
            catch(
                (mi_call(Body, InitialState, _FinalState),
                 extract_tool_result_simple(TotalArity, InputArity, HeadArgs, ToolResult),
                 engine_yield(tool_finished(ToolResult))),
                Error,
                (handle_tool_engine_error(Error, ErrResult),
                 engine_yield(tool_finished(ErrResult)))
            )
        ),
        engine_create(_, Goal, Engine),
        assertz(tool_engine(ToolEngineId, Engine))
    ;   % Tool not found - create engine that just yields error
        engine_create(_, engine_yield(tool_finished(error{message: "Tool not found"})), Engine),
        assertz(tool_engine(ToolEngineId, Engine))
    ).

%% handle_tool_engine_error(+Error, -Result)
handle_tool_engine_error('$answer_commit', true) :- !.
handle_tool_engine_error(Error, error{message: ErrMsg}) :-
    format(atom(ErrMsg), '~w', [Error]).

%% step_tool_engine(+ToolEngineId, -Status, -Content, -Payload)
%% Steps the tool engine, similar to step_engine but for tools.
step_tool_engine(ToolEngineId, Status, Content, Payload) :-
    (   tool_engine(ToolEngineId, Engine)
    ->  catch(
            (   engine_next(Engine, Result)
            ->  process_tool_engine_result(Result, Status, Content, Payload)
            ;   % Engine exhausted without yielding - treat as failure
                Status = finished,
                Content = '',
                Payload = payload{result: false}
            ),
            Error,
            (   format(atom(ErrMsg), 'Tool engine error: ~w', [Error]),
                Status = error,
                Content = ErrMsg,
                Payload = none
            )
        )
    ;   Status = error,
        Content = 'No tool engine found',
        Payload = none
    ).

%% process_tool_engine_result(+Result, -Status, -Content, -Payload)
process_tool_engine_result(tool_finished(ToolResult), finished, '', payload{result: ToolResult}) :- !.
process_tool_engine_result(request_exec(Tool, Args), request_exec, '',
    payload{toolName: Tool, args: Args}) :- !.
process_tool_engine_result(request_sample_token(Prompt, Allowed), request_sample_token, '',
    payload{prompt: Prompt, allowedTokens: Allowed}) :- !.
process_tool_engine_result(request_llm(Messages), request_llm, '',
    payload{messages: Messages}) :- !.
process_tool_engine_result(request_judgment(Payload), request_judgment, '', Payload) :- !.
process_tool_engine_result(request_agent_loop(Desc, Vars, Tools, Memory, ToolScope, ReasoningEffort, RecipeContext), request_agent_loop, '',
    payload{taskDescription: Desc, outputVars: Vars, userTools: Tools, memory: Memory, toolScope: ToolScope, reasoningEffort: ReasoningEffort, recipeContext: RecipeContext}) :- !.
%% Legacy 5-arg format (no reasoning/recipe)
process_tool_engine_result(request_agent_loop(Desc, Vars, Tools, Memory, ToolScope), request_agent_loop, '',
    payload{taskDescription: Desc, outputVars: Vars, userTools: Tools, memory: Memory, toolScope: ToolScope, reasoningEffort: none, recipeContext: ""}) :- !.
%% Legacy 4-arg format (no tool scope)
process_tool_engine_result(request_agent_loop(Desc, Vars, Tools, Memory), request_agent_loop, '',
    payload{taskDescription: Desc, outputVars: Vars, userTools: Tools, memory: Memory, toolScope: none, reasoningEffort: none, recipeContext: ""}) :- !.
%% Process primitives (self/send/receive/spawn/...) reached inside a DML tool
%% are yielded to the host exactly as in the main engine.
process_tool_engine_result(request_proc(Request), request_proc, '', Payload) :-
    proc_request_payload(Request, Payload).
process_tool_engine_result(output(Text), output, Text, none) :- !.
process_tool_engine_result(log(Text), log, Text, none) :- !.
process_tool_engine_result(Other, error, Msg, none) :-
    format(atom(Msg), 'Unknown tool engine result: ~w', [Other]).

%% destroy_tool_engine(+ToolEngineId)
destroy_tool_engine(ToolEngineId) :-
    (   tool_engine(ToolEngineId, Engine)
    ->  catch(engine_destroy(Engine), _, true)
    ;   true
    ),
    retractall(tool_engine(ToolEngineId, _)).

%% post_tool_agent_result(+SessionId, +ToolEngineId, +Success, +Variables, +Messages)
%% Legacy form without a full memory replacement snapshot.
post_tool_agent_result(SessionId, ToolEngineId, Success, Variables, Messages) :-
    post_tool_agent_result(SessionId, ToolEngineId, Success, Variables, Messages, none).

%% post_tool_agent_result(+SessionId, +ToolEngineId, +Success, +Variables, +Messages, +FullMemory)
%% Posts the result of a nested agent loop back to the tool engine.
%% Also stores in session_agent_result so the meta-interpreter can retrieve it.
:- dynamic tool_engine_agent_result/2.  % tool_engine_agent_result(ToolEngineId, Result)

post_tool_agent_result(SessionId, ToolEngineId, Success, Variables, Messages, FullMemory) :-
    Result = result{success: Success, variables: Variables, messages: Messages, full_memory: FullMemory},
    % Store in tool_engine_agent_result (for future use)
    retractall(tool_engine_agent_result(ToolEngineId, _)),
    assertz(tool_engine_agent_result(ToolEngineId, Result)),
    % Also store in session_agent_result so task() in the meta-interpreter can read it
    retractall(session_agent_result(SessionId, _)),
    assertz(session_agent_result(SessionId, Result)),
    % Signal the tool engine that agent result is ready
    (   tool_engine(ToolEngineId, Engine)
    ->  catch(engine_post(Engine, agent_done), _, true)
    ;   true
    ).

%% Helper predicates (shared with old isolated execution)

%% extract_tool_result_simple(+TotalArity, +InputArity, +HeadArgs, -Result)
extract_tool_result_simple(TotalArity, InputArity, HeadArgs, Result) :-
    (   TotalArity > InputArity,
        last(HeadArgs, LastArg),
        nonvar(LastArg)
    ->  Result = LastArg
    ;   Result = true
    ).

%% fill_args(+HeadArgs, +InputArgs, +Count)
%% Fill the first Count elements of HeadArgs with InputArgs
fill_args(_, [], 0) :- !.
fill_args([H|T], [A|As], N) :-
    N > 0,
    H = A,
    N1 is N - 1,
    fill_args(T, As, N1).

%% bind_tool_params(+ParamsList, +ArgsDict)
%% Binds tool parameters from the arguments dictionary
bind_tool_params([], _) :- !.
bind_tool_params([Param|Rest], Args) :-
    (   atom(Param)
    ->  ParamName = Param, ParamVar = Param
    ;   Param = (ParamName = ParamVar)
    ),
    (   get_dict(ParamName, Args, Value)
    ->  ParamVar = Value
    ;   true  % Leave unbound if not provided
    ),
    bind_tool_params(Rest, Args).

%% ============================================================
%% Task Handling
%% ============================================================

%% collect_user_tools(+SessionId, -ToolSchemas)
collect_user_tools(SessionId, ToolSchemas) :-
    findall(
        tool_info{name: Name, schema: Schema, source: Source},
        (
            session_user_tool_schema(SessionId, Name, Schema),
            (SessionId:tool_source(Name, Source) -> true ; Source = "")
        ),
        ToolSchemas
    ).

%% mi_call(task(Desc), +StateIn, -StateOut)
mi_call(task(Desc), StateIn, StateOut) :-
    !,
    consume_gas(StateIn, State0),
    get_params(State0, Params),
    interpolate_desc(Desc, Params, InterpDesc),
    get_session_id(SessionId),
    get_depth(StateIn, Depth),
    add_trace_entry(SessionId, llm_call, task, [InterpDesc], Depth),
    collect_user_tools(SessionId, UserTools),
    get_memory(StateIn, Memory),
    get_tool_scope(StateIn, ToolScope),
    get_reasoning_effort(StateIn, ReasoningEffort),
    get_recipe_context(StateIn, RecipeContext),
    get_model_spec(StateIn, ModelSpec),
    % Yield request with current memory and tool scope
    engine_yield(request_agent_loop(InterpDesc, [], UserTools, Memory, ToolScope, ReasoningEffort, RecipeContext, ModelSpec)),
    % Handle signals (tool calls) until agent_done
    handle_agent_signals(SessionId, StateIn, StateAfterAgent),
    session_agent_result(SessionId, Result),
    retract(session_agent_result(SessionId, _)),
    % Check success and warn on failure
    (   Result.success == true
    ->  true
    ;   (   get_dict(error, Result, ErrorMsg), ErrorMsg \= ""
        ->  format(atom(WarnMsg), 'Warning: task/1 failed: ~w', [ErrorMsg])
        ;   format(atom(WarnMsg), 'Warning: task/1 failed (success=false)', [])
        ),
        engine_yield(output(WarnMsg)),
        fail
    ),
    % Use full memory from agent loop result when available
    (   get_dict(full_memory, Result, FullMemory), is_list(FullMemory)
    ->  set_memory(StateAfterAgent, FullMemory, StateOut),
        add_trace_entry(SessionId, exit, task, [InterpDesc], Depth)
    ;   get_dict(messages, Result, Messages), Messages \= []
    ->  % Preserve system messages from current memory (agent only returns user/assistant)
        get_memory(StateAfterAgent, OldMemory),
        include(is_system_memory_message, OldMemory, SystemMessages),
        append(SystemMessages, Messages, NewMemory),
        set_memory(StateAfterAgent, NewMemory, StateOut),
        % Add exit trace for task/1
        add_trace_entry(SessionId, exit, task, [InterpDesc], Depth)
    ;   % Fallback: just add task description and response (old behavior)
        add_memory(StateAfterAgent, user, InterpDesc, State1),
        (   get_dict(response, Result, Response), Response \= ""
        ->  add_trace_with_result(SessionId, exit, task, [InterpDesc], Response, Depth),
            add_memory(State1, assistant, Response, StateOut)
        ;   add_trace_entry(SessionId, exit, task, [InterpDesc], Depth),
            StateOut = State1
        )
    ).

%% mi_call(task_named(Desc, Vars, VarNames), +StateIn, -StateOut)
%% New unified handler for task/N with embedded variable names
mi_call(task_named(Desc, Vars, VarNames), StateIn, StateOut) :-
    !,
    consume_gas(StateIn, State1),
    mi_call_task_n(Desc, Vars, VarNames, State1, StateOut).

%% ============================================================
%% Prompt Handling - Fresh LLM call without existing memory
%% ============================================================

%% mi_call(sample_token(Desc, Token), +StateIn, -StateOut)
%% Ask the host runtime for exactly one token via the provider.
mi_call(sample_token(Desc, Token), StateIn, StateOut) :-
    !,
    mi_call_sample_token(Desc, none, Token, StateIn, StateOut).

%% mi_call(sample_token(Desc, Allowed, Token), +StateIn, -StateOut)
%% Same as sample_token/2, but provides an allowed token list to runtime.
mi_call(sample_token(Desc, Allowed, Token), StateIn, StateOut) :-
    !,
    mi_call_sample_token(Desc, Allowed, Token, StateIn, StateOut).

%% mi_call(prompt(Desc), +StateIn, -StateOut)
%% Simple LLM call with empty memory and NO tools (pure text completion)
mi_call(prompt(Desc), StateIn, StateOut) :-
    !,
    get_params(StateIn, Params),
    interpolate_desc(Desc, Params, InterpDesc),
    get_session_id(SessionId),
    get_depth(StateIn, Depth),
    add_trace_entry(SessionId, llm_call, prompt, [InterpDesc], Depth),
    % prompt/N is a simple LLM call - no tools, no tool scope
    get_model_spec(StateIn, ModelSpec),
    engine_yield(request_agent_loop(InterpDesc, [], [], [], none, ModelSpec)),
    % Wait for agent_done (no tool calls expected since no tools provided)
    handle_agent_signals(SessionId, StateIn, StateOut),
    session_agent_result(SessionId, Result),
    retract(session_agent_result(SessionId, _)),
    % Check success and warn on failure
    (   Result.success == true
    ->  add_trace_entry(SessionId, exit, prompt, [InterpDesc], Depth)
    ;   (   get_dict(error, Result, ErrorMsg), ErrorMsg \= ""
        ->  format(atom(WarnMsg), 'Warning: prompt/1 failed: ~w', [ErrorMsg])
        ;   format(atom(WarnMsg), 'Warning: prompt/1 failed (success=false)', [])
        ),
        add_trace_entry(SessionId, fail, prompt, [InterpDesc], Depth),
        engine_yield(output(WarnMsg)),
        fail
    ).
    % Don't modify memory - but state may have changed from tool calls

%% mi_call(prompt_named(Desc, Vars, VarNames), +StateIn, -StateOut)
%% New unified handler for prompt/N with embedded variable names
mi_call(prompt_named(Desc, Vars, VarNames), StateIn, StateOut) :-
    !,
    consume_gas(StateIn, State1),
    mi_call_prompt_n(Desc, Vars, VarNames, State1, StateOut).

%% mi_call_prompt_n(+Desc, +Vars, +VarNames, +StateIn, -StateOut)
%% Simple LLM call with empty memory and NO tools (pure text completion with variable binding)
mi_call_prompt_n(Desc, Vars, VarNames, StateIn, StateOut) :-
    !,  % Cut to make this predicate deterministic - no backtracking once started
    get_params(StateIn, Params),
    interpolate_desc(Desc, Params, InterpDesc),
    get_session_id(SessionId),
    get_depth(StateIn, Depth),
    add_trace_entry(SessionId, llm_call, prompt, [InterpDesc], Depth),
    % prompt/N is a simple LLM call - no tools, no tool scope
    get_model_spec(StateIn, ModelSpec),
    resolve_schema_fields(SessionId, VarNames, ResolvedVarNames),
    engine_yield(request_agent_loop(InterpDesc, ResolvedVarNames, [], [], none, ModelSpec)),
    % Wait for agent_done (no tool calls expected since no tools provided)
    handle_agent_signals(SessionId, StateIn, StateOut),
    session_agent_result(SessionId, Result),
    retract(session_agent_result(SessionId, _)),
    % Check success and warn on failure
    (   Result.success == true
    ->  add_trace_entry(SessionId, exit, prompt, [InterpDesc], Depth)
    ;   length(VarNames, Arity),
        ActualArity is Arity + 1,
        (   get_dict(error, Result, ErrorMsg), ErrorMsg \= ""
        ->  format(atom(WarnMsg), 'Warning: prompt/~w failed: ~w', [ActualArity, ErrorMsg])
        ;   format(atom(WarnMsg), 'Warning: prompt/~w failed (success=false)', [ActualArity])
        ),
        add_trace_entry(SessionId, fail, prompt, [InterpDesc], Depth),
        engine_yield(output(WarnMsg)),
        fail
    ),
    bind_task_variables(SessionId, Result.variables, VarNames, Vars).
    % Don't modify memory - but state may have changed from tool calls

mi_call_sample_token(Desc, Allowed, Token, StateIn, StateOut) :-
    get_params(StateIn, Params),
    interpolate_desc(Desc, Params, InterpDesc),
    get_session_id(SessionId),
    get_depth(StateIn, Depth),
    add_trace_entry(SessionId, llm_call, sample_token, [InterpDesc, Allowed], Depth),
    get_model_spec(StateIn, ModelSpec),
    engine_yield(request_sample_token(InterpDesc, Allowed, ModelSpec)),
    wait_sample_token_signal(SessionId),
    (   session_sample_token_result(SessionId, Result)
    ->  retract(session_sample_token_result(SessionId, _)),
        (   Result.status == success
        ->  Token = Result.token,
            add_trace_with_result(SessionId, exit, sample_token, [InterpDesc, Allowed], Token, Depth),
            StateOut = StateIn
        ;   (   get_dict(token, Result, ErrorMsg), ErrorMsg \= ""
            ->  format(atom(WarnMsg), 'Warning: sample_token failed: ~w', [ErrorMsg])
            ;   format(atom(WarnMsg), 'Warning: sample_token failed (status=~w)', [Result.status])
            ),
            add_trace_entry(SessionId, fail, sample_token, [InterpDesc, Allowed], Depth),
            engine_yield(output(WarnMsg)),
            fail
        )
    ;   add_trace_entry(SessionId, fail, sample_token, [InterpDesc, Allowed], Depth),
        engine_yield(output('Warning: sample_token failed (no result returned)')),
        fail
    ).

wait_sample_token_signal(SessionId) :-
    get_next_signal(SessionId, Signal),
    (   Signal == sample_token_done
    ->  true
    ;   Signal = error_signal(_)
    ->  true
    ;   wait_sample_token_signal(SessionId)
    ).

%% ============================================================
%% Semantic Judgment Predicates
%% ============================================================
%% judge/2 asks one batch of typed questions about an explicit state and binds
%% the answers into output variables carried by the spec terms. The one-off
%% predicates are sugar over the same request.
%%
%%   judge(Message, [
%%       choose("Which team?", [billing, orders, account]) - Team,
%%       rate("How frustrated?", [calm, frustrated, angry]) - Frustration,
%%       verify("Does it ask for a refund?") - Refund
%%   ]).
%%
%% Backends (llm, jev, mock) are selected with with_judgment/2, the workspace
%% configuration, or a per-run override. require_judgment/2 gates a goal on a
%% backend capability such as calibrated.

mi_call(judge(State, Specs), StateIn, StateOut) :-
    !,
    mi_call_judgment(State, Specs, StateIn, StateOut).

mi_call(choose(State, Question, Options, Choice), StateIn, StateOut) :-
    !,
    mi_call_judgment(State, [choose(Question, Options) - Choice], StateIn, StateOut).

mi_call(rate(State, Question, Levels, Level), StateIn, StateOut) :-
    !,
    mi_call_judgment(State, [rate(Question, Levels) - Level], StateIn, StateOut).

mi_call(verify(State, Question, Truth), StateIn, StateOut) :-
    !,
    mi_call_judgment(State, [verify(Question) - Truth], StateIn, StateOut).

mi_call(probability(State, Question, Probability), StateIn, StateOut) :-
    !,
    mi_call_judgment(State, [probability(Question) - Probability], StateIn, StateOut).

mi_call(holds(State, Question), StateIn, StateOut) :-
    !,
    mi_call_judgment(State, [verify(Question) - Truth], StateIn, StateOut),
    (   Truth == yes
    ->  true
    ;   fail
    ).

mi_call(holds(State, Question, Threshold), StateIn, StateOut) :-
    !,
    mi_call_judgment(State, [probability(Question) - Probability], StateIn, StateOut),
    (   number(Probability), Probability >= Threshold
    ->  true
    ;   fail
    ).

mi_call(with_judgment(Backend, Goal), StateIn, StateOut) :-
    !,
    set_judge_backend(StateIn, Backend, ScopedState),
    mi_call(Goal, ScopedState, StateWithScope),
    clear_judge_backend(StateWithScope, StateOut).

mi_call(require_judgment(Capability, Goal), StateIn, StateOut) :-
    !,
    push_required_judgment(StateIn, Capability, ScopedState),
    mi_call(Goal, ScopedState, StateWithScope),
    pop_required_judgment(StateWithScope, StateOut).

%% mi_call_judgment(+State, +Specs, +StateIn, -StateOut)
mi_call_judgment(State, Specs, StateIn, StateOut) :-
    consume_gas(StateIn, State1),
    get_session_id(SessionId),
    get_depth(State1, Depth),
    split_specs(Specs, Questions, Outputs),
    length(Questions, Count),
    add_trace_entry(SessionId, llm_call, judgment, [Count], Depth),
    get_judge_backend(State1, Backend),
    get_required_judgments(State1, Requires),
    engine_yield(request_judgment(payload{
        state: State,
        questions: Questions,
        backend: Backend,
        requires: Requires
    })),
    wait_judgment_signal(SessionId),
    (   session_judgment_result(SessionId, Result)
    ->  retract(session_judgment_result(SessionId, _)),
        (   Result.status == success
        ->  add_trace_with_result(SessionId, exit, judgment, [Count], Result.values, Depth),
            bind_judgment_outputs(Outputs, Result.values),
            StateOut = State1
        ;   (   get_dict(error, Result, ErrorMsg), ErrorMsg \= ""
            ->  format(atom(WarnMsg), 'Warning: judgment failed: ~w', [ErrorMsg])
            ;   format(atom(WarnMsg), 'Warning: judgment failed (status=~w)', [Result.status])
            ),
            add_trace_entry(SessionId, fail, judgment, [Count], Depth),
            engine_yield(output(WarnMsg)),
            fail
        )
    ;   add_trace_entry(SessionId, fail, judgment, [Count], Depth),
        engine_yield(output('Warning: judgment failed (no result returned)')),
        fail
    ).

wait_judgment_signal(SessionId) :-
    get_next_signal(SessionId, Signal),
    (   Signal == judgment_done
    ->  true
    ;   Signal = error_signal(_)
    ->  true
    ;   wait_judgment_signal(SessionId)
    ).

%% split_specs(+Specs, -Questions, -Outputs)
%% Each spec is `Question - Output`. Outputs are bound after the batch.
split_specs([], [], []).
split_specs([Spec-Output|Rest], [Question|Questions], [Output|Outputs]) :-
    spec_question(Spec, Question),
    split_specs(Rest, Questions, Outputs).

spec_question(choose(Instruction, Options),
    question{kind: choose, instruction: Instruction, options: Options}).
spec_question(rate(Instruction, Levels),
    question{kind: rate, instruction: Instruction, levels: Levels}).
spec_question(verify(Instruction),
    question{kind: verify, instruction: Instruction}).
spec_question(verify(Instruction, Criteria),
    question{kind: verify, instruction: Instruction, criteria: Criteria}).
spec_question(probability(Instruction),
    question{kind: probability, instruction: Instruction}).

bind_judgment_outputs([], []).
bind_judgment_outputs([Output|Outputs], [Value|Values]) :-
    Output = Value,
    bind_judgment_outputs(Outputs, Values).

%% get_judge_backend(+State, -Backend)
get_judge_backend(State, Backend) :-
    (   get_dict(judge_backend, State, Scoped)
    ->  Backend = Scoped
    ;   Backend = default
    ).

set_judge_backend(StateIn, Backend, StateOut) :-
    StateOut = StateIn.put(judge_backend, Backend).

clear_judge_backend(StateIn, StateOut) :-
    (   get_dict(judge_backend, StateIn, _)
    ->  del_dict(judge_backend, StateIn, _, StateOut)
    ;   StateOut = StateIn
    ).

get_required_judgments(State, Requires) :-
    (   get_dict(judgment_requires, State, Requires)
    ->  true
    ;   Requires = []
    ).

push_required_judgment(StateIn, Capability, StateOut) :-
    get_required_judgments(StateIn, Existing),
    (   member(Capability, Existing)
    ->  StateOut = StateIn
    ;   append(Existing, [Capability], Updated),
        StateOut = StateIn.put(judgment_requires, Updated)
    ).

pop_required_judgment(StateIn, StateOut) :-
    (   get_dict(judgment_requires, StateIn, _)
    ->  del_dict(judgment_requires, StateIn, _, StateOut)
    ;   StateOut = StateIn
    ).

mi_call(llm(Messages, Reply), StateIn, StateOut) :-
    !,
    consume_gas(StateIn, State1),
    mi_call_llm(Messages, Reply, State1, StateOut).

mi_call_llm(Messages, Reply, StateIn, StateOut) :-
    get_session_id(SessionId),
    get_depth(StateIn, Depth),
    add_trace_entry(SessionId, llm_call, llm, [Messages], Depth),
    get_model_spec(StateIn, ModelSpec),
    engine_yield(request_llm(Messages, ModelSpec)),
    wait_llm_signal(SessionId),
    (   session_llm_result(SessionId, Result)
    ->  retract(session_llm_result(SessionId, _)),
        (   Result.status == success
        ->  Reply = Result.text,
            add_trace_with_result(SessionId, exit, llm, [Messages], Reply, Depth),
            StateOut = StateIn
        ;   (   get_dict(text, Result, ErrorMsg), ErrorMsg \= ""
            ->  format(atom(WarnMsg), 'Warning: llm/2 failed: ~w', [ErrorMsg])
            ;   format(atom(WarnMsg), 'Warning: llm/2 failed (status=~w)', [Result.status])
            ),
            add_trace_entry(SessionId, fail, llm, [Messages], Depth),
            engine_yield(output(WarnMsg)),
            fail
        )
    ;   add_trace_entry(SessionId, fail, llm, [Messages], Depth),
        engine_yield(output('Warning: llm/2 failed (no result returned)')),
        fail
    ).

wait_llm_signal(SessionId) :-
    get_next_signal(SessionId, Signal),
    (   Signal == llm_done
    ->  true
    ;   Signal = error_signal(_)
    ->  true
    ;   wait_llm_signal(SessionId)
    ).

%% mi_call_task_n(+Desc, +Vars, +VarNames, +StateIn, -StateOut)
mi_call_task_n(Desc, Vars, VarNames, StateIn, StateOut) :-
    !,  % Cut to make this predicate deterministic - no backtracking once started
    get_params(StateIn, Params),
    interpolate_desc(Desc, Params, InterpDesc),
    get_session_id(SessionId),
    get_depth(StateIn, Depth),
    add_trace_entry(SessionId, llm_call, task, [InterpDesc], Depth),
    collect_user_tools(SessionId, UserTools),
    get_memory(StateIn, Memory),
    get_tool_scope(StateIn, ToolScope),
    get_reasoning_effort(StateIn, ReasoningEffort),
    get_recipe_context(StateIn, RecipeContext),
    get_model_spec(StateIn, ModelSpec),
    resolve_schema_fields(SessionId, VarNames, ResolvedVarNames),
    engine_yield(request_agent_loop(InterpDesc, ResolvedVarNames, UserTools, Memory, ToolScope, ReasoningEffort, RecipeContext, ModelSpec)),
    % Handle signals (tool calls) until agent_done
    handle_agent_signals(SessionId, StateIn, StateAfterAgent),
    session_agent_result(SessionId, Result),
    retract(session_agent_result(SessionId, _)),
    % Check success and warn on failure
    (   Result.success == true
    ->  true
    ;   length(VarNames, Arity),
        ActualArity is Arity + 1,
        (   get_dict(error, Result, ErrorMsg), ErrorMsg \= ""
        ->  format(atom(WarnMsg), 'Warning: task/~w failed: ~w', [ActualArity, ErrorMsg])
        ;   format(atom(WarnMsg), 'Warning: task/~w failed (success=false)', [ActualArity])
        ),
        engine_yield(output(WarnMsg)),
        fail
    ),
    bind_task_variables(SessionId, Result.variables, VarNames, Vars),
    % Use full memory from agent loop result when available
    (   get_dict(full_memory, Result, FullMemory), is_list(FullMemory)
    ->  set_memory(StateAfterAgent, FullMemory, StateOut),
        % Add exit trace for task_n
        add_trace_entry(SessionId, exit, task, [InterpDesc], Depth)
    ;   get_dict(messages, Result, Messages), Messages \= []
    ->  % Preserve system messages from current memory (agent only returns user/assistant)
        get_memory(StateAfterAgent, OldMemory),
        include(is_system_memory_message, OldMemory, SystemMessages),
        append(SystemMessages, Messages, NewMemory),
        set_memory(StateAfterAgent, NewMemory, StateOut),
        % Add exit trace for task_n
        add_trace_entry(SessionId, exit, task, [InterpDesc], Depth)
    ;   % Fallback: just add task description and response (old behavior)
        add_memory(StateAfterAgent, user, InterpDesc, State1),
        (   get_dict(response, Result, Response), Response \= ""
        ->  add_memory(State1, assistant, Response, StateOut)
        ;   StateOut = State1
        )
    ).

%% ---------------------------------------------------------------
%% Output schemas: schema(Name, [field(Field, Type, Option), ...])
%%
%% `list(object(B))` says "a list of dicts" but not what is in them, so every
%% harness re-derives defaults and tolerates missing fields by hand. A schema
%% declares the shape once, and `schema_fill/3` makes the value match it: absent
%% fields with a default are filled, unknown fields are kept (so a model mistake
%% stays visible rather than disappearing), and a `required` field that is absent
%% or a field of the wrong type **fails the step** rather than yielding an empty
%% result.
%%
%%   schema(author, [
%%       field(name,        string, required),
%%       field(affiliation, string, default("not stated")),
%%       field(email,       string, default(""))
%%   ]).
%%
%%   task("From the front matter extract every author: {Text}",
%%        list(schema(author, Authors))).
%% schema_fill(+Fields, +Value, -Filled): the pure part, so it is testable and
%% usable without a session. The session predicate schema_fill/3 looks the fields
%% up from `schema/2`.
schema_fill_fields(Fields, Value, Filled) :-
    is_dict(Value),
    (   member(field(Field, Type, required), Fields),
        \+ dict_field(Value, Field, _),
        !,
        format(atom(Msg), 'schema: required field "~w" is missing', [Field]),
        throw(error(schema_error(Msg), schema_fill/3))
    ;   true
    ),
    (   member(field(Field, Type, _), Fields),
        dict_field(Value, Field, Found),
        \+ schema_value_ok(Type, Found),
        !,
        format(atom(Msg), 'schema: field "~w" is not ~w (~w)', [Field, Type, Found]),
        throw(error(schema_error(Msg), schema_fill/3))
    ;   true
    ),
    dict_pairs(Value, _, Pairs),
    findall(Key-NewValue,
        (   member(Key-Old, Pairs),
            (   member(field(Key, _, Option), Fields)
            ->  (   Option = default(Default), Old == "" -> NewValue = Default
                ;   NewValue = Old
                )
            ;   NewValue = Old          % undeclared: kept, so it stays visible
            )
        ),
        Updated),
    findall(Field-Default,
        (   member(field(Field, _, default(Default)), Fields),
            \+ dict_field(Value, Field, _)
        ),
        Missing),
    append(Updated, Missing, All),
    dict_pairs(Filled, _, All).

%% A declared type accepts its own shape; anything coarser (a number for a string
%% field, for instance) fails, because silently coercing hides the model's error.
schema_value_ok(string, V) :- ( string(V) ; atom(V) ), !.
schema_value_ok(integer, V) :- integer(V), !.
schema_value_ok(number, V) :- number(V), !.
schema_value_ok(boolean, V) :- ( V == true ; V == false ; V == yes ; V == no ), !.
schema_value_ok(object, V) :- is_dict(V), !.
schema_value_ok(array, V) :- is_list(V), !.
schema_value_ok(_, _) :- fail.

%% bind_task_variables(+VarsDict, +Names, -Values)
bind_task_variables(_, _, [], []) :- !.
bind_task_variables(SessionId, VarsDict, [N|Names], [Value|Values]) :-
    (   is_dict(N)
    ->  Key = N.name
    ;   Key = N
    ),
    (   get_dict(Key, VarsDict, Raw)
    ->  true
    ;   Raw = _Missing
    ),
    (   is_dict(N), get_dict(schema, N, SchemaName), nonvar(Raw)
    ->  schema_fields_of(SessionId, SchemaName, Fields),
        (   is_list(Raw)
        ->  maplist(schema_filled(Fields), Raw, Value)
        ;   schema_fill_fields(Fields, Raw, Value)
        )
    ;   Value = Raw
    ),
    bind_task_variables(SessionId, VarsDict, Names, Values).

schema_fields_of(SessionId, Name, Fields) :-
    (   catch(clause(SessionId:schema(Name, Fields), true), _, fail)
    ->  true
    ;   format(atom(Msg), 'schema "~w" is not declared', [Name]),
        throw(error(schema_error(Msg), schema_fields_of/3))
    ).

%% resolve_schema_fields(+SessionId, +VarNames, -Resolved)
%%
%% Attach a schema's field list to the descriptor for each output variable that
%% names one, so the agent building the prompt can tell the model the shape to
%% produce. Done here rather than in extract_var_info/4 (which runs at load time)
%% because the `schema/2` fact is only certainly present once everything is loaded.
resolve_schema_fields(SessionId, VarNames, Resolved) :-
    maplist(resolve_schema_field(SessionId), VarNames, Resolved).

resolve_schema_field(SessionId, VarName, Resolved) :-
    (   is_dict(VarName), get_dict(schema, VarName, SchemaName)
    ->  (   schema_field_dicts(SessionId, SchemaName, Fields)
        ->  put_dict(fields, VarName, Fields, Resolved)
        ;   Resolved = VarName
        )
    ;   Resolved = VarName
    ).

%% Field descriptors as dicts, so they cross the bridge without a decoder.
%% `required` is 1/0 rather than an atom: the atom `false` arrives as the string
%% "false", which is truthy in JavaScript.
schema_field_dicts(SessionId, SchemaName, Dicts) :-
    catch(clause(SessionId:schema(SchemaName, Fields), true), _, fail),
    findall(x{name: Field, type: Type, required: Required, default: Default},
        (   member(field(Field, Type, Option), Fields),
            (   Option == required -> Required = 1 ; Required = 0 ),
            (   Option = default(D) -> Default = D ; Default = no_default )
        ),
        Dicts).

schema_filled(Fields, Raw, Filled) :-
    (   is_dict(Raw) -> schema_fill_fields(Fields, Raw, Filled) ; Filled = Raw ).

%% ============================================================
%% Exec Handling
%% ============================================================

%% mi_call(exec(ToolCall, Output), +StateIn, -StateOut)
mi_call(exec(ToolCall, Output), StateIn, StateOut) :-
    !,
    consume_gas(StateIn, State0),
    ToolCall =.. [ToolName|Args],
    get_session_id(SessionId),
    get_depth(State0, Depth),
    add_trace_entry(SessionId, exec, ToolName, Args, Depth),
    engine_yield(request_exec(ToolName, Args)),
    % Use get_next_signal helper which handles both dynamic predicate and engine_fetch
    get_next_signal(SessionId, _Signal),
    session_exec_result(SessionId, Result),
    retract(session_exec_result(SessionId, _)),
    (   Result.status == success
    ->  Output = Result.result,
        add_trace_with_result(SessionId, exit, ToolName, Args, Output, Depth),
        StateOut = State0
    ;   add_trace_entry(SessionId, fail, ToolName, Args, Depth),
        fail
    ).

%% ============================================================
%% Process Primitives (session-scoped subagents)
%% ============================================================
%% These yield to the TypeScript host, which owns process identity, mailboxes,
%% links, monitors, and naming. See docs/DML_REPL_PROCESSES_SPEC.md.
%% Goals and messages cross the boundary as canonical term strings, so a
%% process never shares a variable with its spawner (invariant A).

%% mi_call(self(?Pid), +StateIn, -StateOut)
mi_call(self(Pid), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    engine_yield(request_proc(self)),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, self, Pid),
    retract(session_proc_result(SessionId, self, _)).

%% mi_call(spawn(+Goal, -Pid), +StateIn, -StateOut)
mi_call(spawn(Goal, Pid), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    copy_term(Goal, GoalCopy),
    term_string(GoalCopy, GoalText),
    engine_yield(request_proc(spawn(GoalText))),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, spawn, SpawnResult),
    retract(session_proc_result(SessionId, spawn, _)),
    (   SpawnResult = ok(Pid)
    ->  true
    ;   throw(error(process_spawn_failed(SpawnResult), spawn/2))
    ).

%% mi_call(spawn_opt(+Goal, -Pid, +Options), +StateIn, -StateOut)
mi_call(spawn_opt(Goal, Pid, Options), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    copy_term(Goal, GoalCopy),
    term_string(GoalCopy, GoalText),
    (   is_dict(Options) -> term_string(Options, OptionsText) ; OptionsText = "{}" ),
    engine_yield(request_proc(spawn(GoalText, OptionsText))),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, spawn, SpawnResult),
    retract(session_proc_result(SessionId, spawn, _)),
    (   SpawnResult = ok(Pid)
    ->  true
    ;   throw(error(process_spawn_failed(SpawnResult), spawn_opt/3))
    ).

%% mi_call(send(+Target, +Message), +StateIn, -StateOut)
mi_call(send(To, Message), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    term_string(To, ToText),
    term_string(Message, MessageText),
    engine_yield(request_proc(send(ToText, MessageText))),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, send, SendResult),
    retract(session_proc_result(SessionId, send, _)),
    (   SendResult == ok
    ->  true
    ;   throw(error(process_send_failed(SendResult), send/2))
    ).

%% mi_call(receive(?Message), +StateIn, -StateOut)
mi_call(receive(Message), StateIn, StateIn) :-
    !,
    mi_call(receive(Message, 0), StateIn, StateIn).

%% mi_call(receive(?Message, +TimeoutMs), +StateIn, -StateOut)
%% Fails on timeout, so a caller can branch on it.
mi_call(receive(Message, TimeoutMs), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    engine_yield(request_proc(receive(TimeoutMs))),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, receive, ReceiveResult),
    retract(session_proc_result(SessionId, receive, _)),
    ReceiveResult = ok(Message).

%% mi_call(receive_match(+Template, ?Message, +TimeoutMs), +StateIn, -StateOut)
%% Selective receive: skip messages that do not unify with Template, requeue
%% them in order, and fail on timeout. The template unifies in this process.
mi_call(receive_match(Template, Message, TimeoutMs), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    proc_now_ms(Start),
    receive_match_loop(Template, Message, TimeoutMs, Start, SessionId, []).

%% A plain monotonic clock for helpers (now_ms/1 is a mi_call special predicate).
proc_now_ms(Milliseconds) :-
    get_time(Seconds),
    Milliseconds is round(Seconds * 1000).

receive_match_loop(Template, Message, TimeoutMs, Start, SessionId, Skipped) :-
    proc_now_ms(Now),
    (   TimeoutMs > 0
    ->  Left is TimeoutMs - (Now - Start)
    ;   Left = 0
    ),
    (   TimeoutMs > 0, Left =< 0
    ->  requeue_skipped(Skipped, SessionId),
        fail
    ;   engine_yield(request_proc(receive(Left))),
        get_next_signal(SessionId, _),
        session_proc_result(SessionId, receive, Result),
        retract(session_proc_result(SessionId, receive, _)),
        (   Result = ok(Term),
            (   copy_term(Template, Copy), Copy = Term
            ->  requeue_skipped(Skipped, SessionId),
                Template = Term,
                Message = Term
            ;   receive_match_loop(Template, Message, TimeoutMs, Start, SessionId, [Term|Skipped])
            )
        ;   requeue_skipped(Skipped, SessionId),
            fail
        )
    ).

requeue_skipped(Skipped, SessionId) :-
    requeue_reversed(Skipped, [], Reversed),
    requeue_all(Reversed, SessionId).

%% Build the reversed list without library(lists) (deepclause_mi may not have it).
requeue_reversed([], Acc, Acc).
requeue_reversed([X|Xs], Acc, Out) :- requeue_reversed(Xs, [X|Acc], Out).

requeue_all([], _).
requeue_all([Message|Messages], SessionId) :-
    term_string(Message, Text),
    engine_yield(request_proc(requeue(Text))),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, requeue, _),
    retract(session_proc_result(SessionId, requeue, _)),
    requeue_all(Messages, SessionId).

%% mi_call(exit(+Reason), +StateIn, -StateOut)
%% Terminate this process. The host marks it killed; the frame is stopped by
%% the throw below, which the runner maps to a clean termination.
mi_call(exit(Reason), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    term_string(Reason, ReasonText),
    engine_yield(request_proc(exit(ReasonText))),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, exit, _),
    retract(session_proc_result(SessionId, exit, _)),
    throw(process_exit(Reason)).

%% mi_call(exit(+Pid, +Reason), +StateIn, -StateOut)
%% Terminate another process. Unlike exit/1 this returns; the target is killed.
mi_call(exit(Pid, Reason), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    term_string(Pid, PidText),
    term_string(Reason, ReasonText),
    engine_yield(request_proc(exit(PidText, ReasonText))),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, exit, ExitResult),
    retract(session_proc_result(SessionId, exit, _)),
    (   ExitResult == ok
    ->  true
    ;   throw(error(process_exit_failed(ExitResult), exit/2))
    ).

%% mi_call(process_info(+Pid, -Info), +StateIn, -StateOut)
mi_call(process_info(Pid, Info), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    term_string(Pid, PidText),
    engine_yield(request_proc(process_info(PidText))),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, process_info, Info),
    retract(session_proc_result(SessionId, process_info, _)).

%% mi_call(register(+Name, +Pid), +StateIn, -StateOut)
mi_call(register(Name, Pid), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    term_string(Name, NameText),
    term_string(Pid, PidText),
    engine_yield(request_proc(register(NameText, PidText))),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, register, RegisterResult),
    retract(session_proc_result(SessionId, register, _)),
    (   RegisterResult == ok
    ->  true
    ;   throw(error(process_register_failed(RegisterResult), register/2))
    ).

%% mi_call(whereis(+Name, ?Pid), +StateIn, -StateOut)
mi_call(whereis(Name, Pid), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    term_string(Name, NameText),
    engine_yield(request_proc(whereis(NameText))),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, whereis, WhereResult),
    retract(session_proc_result(SessionId, whereis, _)),
    WhereResult = ok(Pid).

%% mi_call(sleep(+Milliseconds), +StateIn, -StateOut)
mi_call(sleep(Milliseconds), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    engine_yield(request_proc(sleep(Milliseconds))),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, sleep, _),
    retract(session_proc_result(SessionId, sleep, _)).

%% mi_call(make_ref(-Ref), +StateIn, -StateOut)
mi_call(make_ref(Ref), StateIn, StateIn) :-
    !,
    gensym(ref_, Ref).

%% mi_call(now_ms(-Milliseconds), +StateIn, -StateOut)
mi_call(now_ms(Milliseconds), StateIn, StateIn) :-
    !,
    get_time(Seconds),
    Milliseconds is round(Seconds * 1000).

%% mi_call(serve(+Name, +Goal), +StateIn, -StateOut)
%% Spawn a session-scoped process and register it under Name. Sugar over
%% spawn/2 + register/2; the goal decides the process's shape.
mi_call(serve(Name, Goal), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    copy_term(Goal, GoalCopy),
    term_string(GoalCopy, GoalText),
    engine_yield(request_proc(spawn(GoalText))),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, spawn, SpawnResult),
    retract(session_proc_result(SessionId, spawn, _)),
    SpawnResult = ok(Pid),
    term_string(Name, NameText),
    term_string(Pid, PidText),
    engine_yield(request_proc(register(NameText, PidText))),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, register, _),
    retract(session_proc_result(SessionId, register, _)).

%% mi_call(service_cast(+Target, +Message), +StateIn, -StateOut)
mi_call(service_cast(Target, Message), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    term_string(Target, TargetText),
    term_string(Message, MessageText),
    engine_yield(request_proc(send(TargetText, MessageText))),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, send, _),
    retract(session_proc_result(SessionId, send, _)).

%% mi_call(service_call(+Target, +Request, -Reply), +StateIn, -StateOut)
mi_call(service_call(Target, Request, Reply), StateIn, StateOut) :-
    !,
    mi_call(service_call(Target, Request, Reply, 300000), StateIn, StateOut).

%% mi_call(service_call(+Target, +Request, -Reply, +TimeoutMs), ...)
%% Request/reply against a process using the request(From, Ref, Request) /
%% reply(Ref, Value) protocol. Unrelated messages are ignored while waiting.
mi_call(service_call(Target, Request, Reply, Timeout), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    gensym(reply_ref_, Ref),
    engine_yield(request_proc(self)),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, self, Me),
    retract(session_proc_result(SessionId, self, _)),
    term_string(Target, TargetText),
    term_string(request(Me, Ref, Request), RequestText),
    engine_yield(request_proc(send(TargetText, RequestText))),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, send, _),
    retract(session_proc_result(SessionId, send, _)),
    service_await_reply(Ref, Reply, Timeout).

%% service_await_reply(+Ref, -Reply, +TimeoutMs)
service_await_reply(Ref, Reply, Timeout) :-
    get_session_id(SessionId),
    engine_yield(request_proc(receive(Timeout))),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, receive, Result),
    retract(session_proc_result(SessionId, receive, _)),
    (   Result = ok(reply(Ref, Reply))
    ->  true
    ;   Result = ok(_)
    ->  service_await_reply(Ref, Reply, Timeout)
    ;   fail
    ).

%% mi_call(wait(+Pid[, -Outcome]), +StateIn, -StateOut)
%% Block until a process terminates. Outcome is ok(Result), failed(Error), or
%% killed(Reason); Result is the process's terminal text as a Prolog string.
%% `wait/1` is semidet: it succeeds only on a successful finish.
mi_call(wait(Pid), StateIn, StateIn) :-
    !,
    mi_call(wait(Pid, Outcome), StateIn, _),
    Outcome = ok(_).

mi_call(wait(Pid, Outcome), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    term_string(Pid, PidText),
    engine_yield(request_proc(wait(PidText))),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, wait, Outcome),
    retract(session_proc_result(SessionId, wait, _)).

%% ============================================================
%% Swarm / pmap: fan a worker over a list concurrently
%% ============================================================
%% Each item runs in its own process (spawned through the host), calling
%% call(Worker, Item, Value). Results preserve input order. `Concurrency` is
%% accepted for compatibility and not yet enforced (the host's process limit is).

proc_self(SessionId, Me) :-
    engine_yield(request_proc(self)),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, self, Me),
    retract(session_proc_result(SessionId, self, _)).

%% mi_call(swarm(+Worker, +Items, -Results), ...)
mi_call(swarm(Worker, Items, Results), StateIn, StateOut) :-
    !,
    mi_call(swarm(Worker, Items, Results, 4), StateIn, StateOut).

%% mi_call(swarm(+Worker, +Items, -Results, +Concurrency), ...)
%% Windowed fan-out: at most Concurrency items run at once.
mi_call(swarm(Worker, Items, Results, Concurrency), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    proc_self(SessionId, Me),
    (   integer(Concurrency), Concurrency > 0 -> Conc = Concurrency ; Conc = 4 ),
    swarm_loop(Items, 1, [], Conc, Worker, Me, SessionId, [], Pairs),
    keysort(Pairs, Sorted),
    swarm_values(Sorted, Results).

%% Accumulate `Acc`, bind the collected pairs to `Pairs` at the base.
swarm_loop(Pending, NextIndex, InFlight, Conc, Worker, Me, SessionId, Acc, Pairs) :-
    (   Pending = [Item|Rest],
        length(InFlight, K), K < Conc
    ->  swarm_spawn_one(Item, NextIndex, Worker, Me, SessionId, SpawnResult),
        Next is NextIndex + 1,
        (   SpawnResult = ok(_Pid)
        ->  swarm_loop(Rest, Next, [NextIndex|InFlight], Conc, Worker, Me, SessionId, Acc, Pairs)
        ;   % The host refused the spawn (e.g. the live-process limit): fail only
            % this item, so the swarm still completes and the reason is visible
            % instead of the whole call failing silently.
            swarm_loop(Rest, Next, InFlight, Conc, Worker, Me, SessionId,
                       [NextIndex-failed(SpawnResult)|Acc], Pairs)
        )
    ;   InFlight = [_|_]
    ->  swarm_receive(Index, Value, SessionId),
        selectchk(Index, InFlight, Rest),
        swarm_loop(Pending, NextIndex, Rest, Conc, Worker, Me, SessionId, [Index-Value|Acc], Pairs)
    ;   Pairs = Acc
    ).

swarm_spawn_one(Item, Index, Worker, Me, SessionId, SpawnResult) :-
    term_string(deepclause_mi:dc_swarm_item(Worker, Item, Me, Index), GoalText),
    engine_yield(request_proc(spawn(GoalText))),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, spawn, SpawnResult),
    retract(session_proc_result(SessionId, spawn, _)).

swarm_receive(Index, Value, SessionId) :-
    engine_yield(request_proc(receive(0))),
    get_next_signal(SessionId, _),
    session_proc_result(SessionId, receive, R),
    retract(session_proc_result(SessionId, receive, _)),
    R = ok(swarm_result(Index, Value)).

swarm_values([], []).
swarm_values([_-Value|Rest], [Value|Values]) :- swarm_values(Rest, Values).

%% dc_swarm_item(+Worker, +Item, +Parent, +Index) — the child goal.
%% Catch a thrown error (an undefined worker, a budget, a tool failure) and
%% report it as `failed(Error)`. Without the catch the child process crashes and
%% never sends a result, so the parent's `swarm_receive` waits forever.
%%
%% The `call` must stay inline in this clause: a helper predicate defined in
%% `deepclause_mi` is reached via the meta-interpreter's raw-Prolog fallback, and
%% then `call(Worker, ...)` resolves in `deepclause_mi` instead of the session
%% module (so every worker looked undefined).
dc_swarm_item(Worker, Item, Parent, Index) :-
    catch(
        (   call(Worker, Item, Result)
        ->  Outcome = ok(Result)
        ;   Outcome = failed
        ),
        Error,
        Outcome = failed(Error)
    ),
    send(Parent, swarm_result(Index, Outcome)).

%% mi_call(pmap(+Worker, +Items, -Values), ...)
%% Like swarm/3, but fails if any item failed.
mi_call(pmap(Worker, Items, Values), StateIn, StateIn) :-
    !,
    mi_call(swarm(Worker, Items, Results), StateIn, _),
    pmap_ok(Results, Values).

pmap_ok([], []).
pmap_ok([ok(Value)|Rest], [Value|Values]) :- pmap_ok(Rest, Values).

%% ============================================================
%% Tool Scoping Predicates
%% ============================================================
%% These predicates allow manual control over which tools are available
%% to nested task() calls. They set a scope in the state which is passed
%% to the TypeScript runner and used to filter available tools.
%%
%% Example:
%%   tool(smart_format(Items, Output)) :-
%%       with_tools([summarize, calculate], (
%%           task("Format this nicely", Output)
%%       )).

%% mi_call(with_tools(ToolList, Goal), +StateIn, -StateOut)
%% Run Goal with only the specified tools available to nested tasks
mi_call(with_tools(ToolList, Goal), StateIn, StateOut) :-
    !,
    set_tool_scope(StateIn, whitelist(ToolList), ScopedState),
    mi_call(Goal, ScopedState, StateWithScope),
    clear_tool_scope(StateWithScope, StateOut).

%% mi_call(with_fresh_memory(Goal), +StateIn, -StateOut)
%% Run Goal with an empty memory, and discard whatever it accumulates. The steps
%% inside see nothing from the caller, and the caller sees nothing from them.
%% Use it to scope a group of task/N calls that must not be influenced by the
%% conversation so far, or to keep a long history from growing through a
%% sub-computation. `prompt/N` is the single-call equivalent.
mi_call(with_fresh_memory(Goal), StateIn, StateOut) :-
    !,
    get_memory(StateIn, SavedMemory),
    set_memory(StateIn, [], FreshState),
    mi_call(Goal, FreshState, AfterGoal),
    set_memory(AfterGoal, SavedMemory, StateOut).

%% mi_call(without_tools(ToolList, Goal), +StateIn, -StateOut)
%% Run Goal with the specified tools excluded from nested tasks
mi_call(without_tools(ToolList, Goal), StateIn, StateOut) :-
    !,
    set_tool_scope(StateIn, blacklist(ToolList), ScopedState),
    mi_call(Goal, ScopedState, StateWithScope),
    clear_tool_scope(StateWithScope, StateOut).

%% mi_call(with_reasoning(Goal, Effort), +StateIn, -StateOut)
%% Run Goal with a specific reasoning effort level (none/low/medium/high).
%% The effort is passed to the TypeScript runner via request_agent_loop.
mi_call(with_reasoning(Goal, Effort), StateIn, StateOut) :-
    !,
    set_reasoning_effort_state(StateIn, Effort, ScopedState),
    mi_call(Goal, ScopedState, StateWithScope),
    clear_reasoning_effort_state(StateWithScope, StateOut).

%% mi_call(with_model(Spec, Goal), +StateIn, -StateOut)
%% Run Goal with a specific model, and optionally a reasoning level. `Spec` is a
%% string — "provider:model", "provider:model#reasoning", "model",
%% "model#reasoning" — or a model{provider, id, temperature, maxTokens, reasoning}
%% dict. It applies to task/N, prompt/N and sample_token/N (not judgments).
mi_call(with_model(Spec, Goal), StateIn, StateOut) :-
    !,
    set_model_state(StateIn, Spec, ScopedState),
    mi_call(Goal, ScopedState, StateWithScope),
    clear_model_state(StateWithScope, StateOut).

%% mi_call(with_recipe(Goal, Query), +StateIn, -StateOut)
%% Run Goal with a recipe loaded as transient context (not persisted to memory).
mi_call(with_recipe(Goal, Query), StateIn, StateOut) :-
    !,
    load_recipe_content(Query, StateIn, StateAfterRecipeLoad, RecipeContent),
    set_recipe_context(StateAfterRecipeLoad, RecipeContent, ScopedState),
    mi_call(Goal, ScopedState, StateWithRecipe),
    clear_recipe_context(StateWithRecipe, StateOut).

%% ============================================================
%% Memory Predicates - NOW BACKTRACKABLE VIA STATE!
%% ============================================================

%% mi_call(system(Text), +StateIn, -StateOut)
mi_call(system(Text), StateIn, StateOut) :-
    !,
    get_params(StateIn, Params),
    interpolate_desc(Text, Params, InterpText),
    add_memory(StateIn, system, InterpText, StateOut).

%% mi_call(user(Text), +StateIn, -StateOut)
mi_call(user(Text), StateIn, StateOut) :-
    !,
    get_params(StateIn, Params),
    interpolate_desc(Text, Params, InterpText),
    add_memory(StateIn, user, InterpText, StateOut).

%% mi_call(get_memory(Messages), +StateIn, -StateOut)
mi_call(get_memory(Messages), StateIn, StateIn) :-
    !,
    get_memory(StateIn, Messages).

%% ============================================================
%% Media (see docs/DML_MEDIA.md)
%% ============================================================

%% mi_call(read_media(+Path, -Media), +StateIn, -StateOut)
%% Binds Media = media(Path, Mime) for a workspace-relative image. The bytes
%% stay on disk; only the reference enters memory.
mi_call(read_media(Path, Media), StateIn, StateIn) :-
    !,
    get_params(StateIn, Params),
    interpolate_desc(Path, Params, P),
    media_mime(P, Mime),
    Media = media(P, Mime).

%% mi_call(attach_media(+Media), +StateIn, -StateOut)
%% Appends a user memory message carrying a reference-only media entry, so the
%% next task/N can see the image.
mi_call(attach_media(Media), StateIn, StateOut) :-
    !,
    media_dict(Media, media{path: Path, mime: Mime}),
    format(atom(Note), '[attached ~w: ~w]', [Mime, Path]),
    Message = message{role: user, content: Note, media: [media{path: Path, mime: Mime}]},
    get_memory(StateIn, Memory),
    append(Memory, [Message], NewMemory),
    set_memory(StateIn, NewMemory, StateOut).

%% mi_call(save_image(+Media, +OutPath), +StateIn, -StateOut)
%% Write a media reference to a workspace file: `save_image(media(Path, Mime), "out.png")`,
%% `save_image(media{svg: Svg}, "out.svg")`. Parent directories must exist.
mi_call(save_image(Media, Out), StateIn, StateIn) :-
    !,
    media_dict(Media, Dict),
    (   get_dict(kind, Dict, inline_svg),
        get_dict(data, Dict, Svg)
    ->  open(Out, write, Stream),
        write(Stream, Svg),
        close(Stream)
    ;   get_dict(path, Dict, Path),
        Path \== ""
    ->  copy_file(Path, Out)
    ;   throw(error(domain_error(save_image, Media), save_image/2))
    ).

%% media_dict(+Media, -Dict)
%% Normalizes an image reference for display/attachment:
%%   media(Path, Mime) | media(Path) | a bare Path string
%%   media{path: Path} | media{path: Path, mime: Mime}
%%   media{svg: SvgString}                 (inline SVG)
%%   media{data: Base64} | media{data: Base64, mime: Mime}   (inline bytes)
media_dict(media(Path, Mime), media{path: Path, mime: Mime}) :- !.
media_dict(media(Path), media{path: Path, mime: Mime}) :- !, media_mime(Path, Mime).
media_dict(media{path: Path, mime: Mime}, media{path: Path, mime: Mime}) :- !.
media_dict(media{path: Path}, media{path: Path, mime: Mime}) :- !, media_mime(Path, Mime).
media_dict(media{svg: Svg}, media{path: "", mime: "image/svg+xml", kind: inline_svg, data: Svg}) :- !.
media_dict(media{data: Data, mime: Mime}, media{path: "", mime: Mime, kind: inline_data, data: Data}) :- !.
media_dict(media{data: Data}, media{path: "", mime: "image/png", kind: inline_data, data: Data}) :- !.
media_dict(Path, media{path: Path, mime: Mime}) :- media_mime(Path, Mime).

%% media_mime(+Path, -Mime)
%% Allowed media types, matching pi image content blocks.
media_mime(Path, Mime) :-
    (   string(Path) -> atom_string(Atom, Path) ; Atom = Path ),
    (   sub_atom(Atom, _, _, 0, '.png')  -> Mime = "image/png"
    ;   sub_atom(Atom, _, _, 0, '.svg')  -> Mime = "image/svg+xml"
    ;   sub_atom(Atom, _, _, 0, '.jpg')  -> Mime = "image/jpeg"
    ;   sub_atom(Atom, _, _, 0, '.jpeg') -> Mime = "image/jpeg"
    ;   sub_atom(Atom, _, _, 0, '.gif')  -> Mime = "image/gif"
    ;   sub_atom(Atom, _, _, 0, '.webp') -> Mime = "image/webp"
    ;   fail
    ).

%% ============================================================
%% Output Predicates
%% ============================================================

%% mi_call(answer(Text), +StateIn, -StateOut)
%% answer/1 commits by throwing - prevents backtracking to other clauses
mi_call(answer(Text), StateIn, StateIn) :-
    get_params(StateIn, Params),
    interpolate_desc(Text, Params, InterpText),
    engine_yield(answer(InterpText)),
    throw('$answer_commit').

%% mi_call(output(Media), +StateIn, -StateOut)
%% Present an image instead of text: `output(media(Path, Mime))`, `output(media(Path))`,
%% or a dict form (`media{svg: Svg}`, `media{data: Base64, mime: Mime}`). The host
%% renders it inline (kitty/iTerm2) or reports the path.
%% This clause must come before the generic `output(Text)` clause; it fails
%% through for anything that is not a media reference.
mi_call(output(Media), StateIn, StateIn) :-
    nonvar(Media),
    (   Media = media(_, _)
    ;   Media = media(_)
    ;   is_dict(Media)
    ),
    media_dict(Media, Dict),
    !,
    get_dict(mime, Dict, Mime),
    get_dict(path, Dict, Path),
    (   get_dict(data, Dict, Data) -> true ; Data = "" ),
    (   get_dict(kind, Dict, Kind) -> true ; Kind = file ),
    get_session_id(SessionId),
    get_depth(StateIn, Depth),
    add_trace_entry(SessionId, media, media, [Path], Depth),
    engine_yield(media(media{path: Path, mimeType: Mime, kind: Kind, data: Data})).

%% mi_call(output(Text), +StateIn, -StateOut)
mi_call(output(Text), StateIn, StateIn) :-
    get_params(StateIn, Params),
    interpolate_desc(Text, Params, InterpText),
    get_session_id(SessionId),
    get_depth(StateIn, Depth),
    add_trace_entry(SessionId, output, output, [InterpText], Depth),
    engine_yield(output(InterpText)).

%% mi_call(input(Prompt, Input), +StateIn, -StateOut)
%% Request input from the user with a prompt
mi_call(input(Prompt, Input), StateIn, StateIn) :-
    get_params(StateIn, Params),
    interpolate_desc(Prompt, Params, InterpPrompt),
    get_session_id(SessionId),
    get_depth(StateIn, Depth),
    add_trace_entry(SessionId, input, input, [InterpPrompt], Depth),
    engine_yield(wait_input(InterpPrompt)),
    % Use get_next_signal helper which handles both dynamic predicate and engine_fetch
    get_next_signal(SessionId, _Signal),
    % A cancelled input request fails the goal (non-interactive policy).
    (   session_input_cancelled(SessionId)
    ->  retractall(session_input_cancelled(SessionId)),
        fail
    ;   session_pending_input(SessionId, Input)
    ->  retract(session_pending_input(SessionId, Input)),
        add_trace_with_result(SessionId, exit, input, [InterpPrompt], Input, Depth)
    ;   Input = "",  % Default to empty if no input provided
        add_trace_with_result(SessionId, exit, input, [InterpPrompt], "", Depth)
    ).

%% mi_call(yield(Text), +StateIn, -StateOut)
mi_call(yield(Text), StateIn, StateIn) :-
    get_params(StateIn, Params),
    interpolate_desc(Text, Params, InterpText),
    get_session_id(SessionId),
    get_depth(StateIn, Depth),
    add_trace_entry(SessionId, output, yield, [InterpText], Depth),
    engine_yield(output(InterpText)).

%% mi_call(log(Text), +StateIn, -StateOut)
mi_call(log(Text), StateIn, StateIn) :-
    get_params(StateIn, Params),
    interpolate_desc(Text, Params, InterpText),
    engine_yield(log(InterpText)).

%% mi_call(format(Format, Args), +StateIn, -StateOut)
%% In REPL mode, capture format/2 output as an output event rather than
%% writing to the process stdout. Outside the REPL (runDML), keep normal
%% behavior so existing traces/prompts are unaffected.
mi_call(format(Format, Args), StateIn, StateIn) :-
    get_session_id(SessionId),
    (   session_repl_mode(SessionId, true)
    ->  with_output_to(string(Text), format(Format, Args)),
        mi_call(output(Text), StateIn, StateIn)
    ;   format(Format, Args)
    ).

%% mi_call(writeln(Value), +StateIn, -StateOut)
mi_call(writeln(Value), StateIn, StateIn) :-
    get_session_id(SessionId),
    (   session_repl_mode(SessionId, true)
    ->  with_output_to(string(Text), writeln(Value)),
        mi_call(output(Text), StateIn, StateIn)
    ;   writeln(Value)
    ).

%% ============================================================
%% Context Stack Management - for memory save/restore
%% ============================================================

%% mi_call(push_context, +StateIn, -StateOut)
%% Save current memory to stack, keep memory active
mi_call(push_context, StateIn, StateOut) :-
    !,
    get_memory(StateIn, Memory),
    get_context_stack(StateIn, Stack),
    set_context_stack(StateIn, [Memory|Stack], StateOut).

%% mi_call(push_context(clear), +StateIn, -StateOut)
%% Save current memory to stack AND clear memory
mi_call(push_context(clear), StateIn, StateOut) :-
    !,
    get_memory(StateIn, Memory),
    get_context_stack(StateIn, Stack),
    set_context_stack(StateIn, [Memory|Stack], State1),
    set_memory(State1, [], StateOut).

%% mi_call(pop_context, +StateIn, -StateOut)
%% Restore memory from stack
mi_call(pop_context, StateIn, StateOut) :-
    !,
    get_context_stack(StateIn, Stack),
    (   Stack = [SavedMemory|RestStack]
    ->  set_memory(StateIn, SavedMemory, State1),
        set_context_stack(State1, RestStack, StateOut)
    ;   % Empty stack - do nothing
        StateOut = StateIn
    ).

%% mi_call(clear_memory, +StateIn, -StateOut)
%% Clear current memory without saving
mi_call(clear_memory, StateIn, StateOut) :-
    !,
    set_memory(StateIn, [], StateOut).

%% ============================================================
%% Parameter Handling
%% ============================================================

%% mi_call(param(Key, Value), +StateIn, -StateOut)
%% Simple param/2 lookup from session module (asserted facts)
mi_call(param(Key, Value), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    (   atom(Key)
    ->  KeyAtom = Key
    ;   atom_string(KeyAtom, Key)
    ),
    SessionId:param(KeyAtom, Value).

%% mi_call(param(Key, Desc, Value), +StateIn, -StateOut)
%% Legacy param/3 - still queries from state params dict
mi_call(param(Key, _Desc, Value), StateIn, StateIn) :-
    !,
    get_params(StateIn, Params),
    (   atom(Key)
    ->  KeyAtom = Key
    ;   atom_string(KeyAtom, Key)
    ),
    get_dict(KeyAtom, Params, Value).

%% ============================================================
%% Control Flow - STATE THREADED
%% No cuts - rely on is_mi_special_predicate guard in catch-all
%% ============================================================

%% mi_call((A, B), +StateIn, -StateOut)
mi_call((A, B), StateIn, StateOut) :-
    mi_call(A, StateIn, State1),
    mi_call(B, State1, StateOut).

%% mi_call((Cond -> Then ; Else), +StateIn, -StateOut)
%% MUST come before disjunction clause - if-then-else is syntactically a disjunction!
mi_call((Cond -> Then ; Else), StateIn, StateOut) :-
    !,  % Commit to this clause for if-then-else patterns
    (   mi_call(Cond, StateIn, State1)
    ->  mi_call(Then, State1, StateOut)
    ;   mi_call(Else, StateIn, StateOut)
    ).

%% mi_call((Cond -> Then), +StateIn, -StateOut)
%% Soft cut without else branch
mi_call((Cond -> Then), StateIn, StateOut) :-
    !,  % Commit to this clause
    (   mi_call(Cond, StateIn, State1)
    ->  mi_call(Then, State1, StateOut)
    ).

%% mi_call((A ; B), +StateIn, -StateOut)
%% Disjunction - BACKTRACKABLE! On failure, StateIn is restored
%% NOTE: This clause must come AFTER if-then-else clauses
mi_call((A ; B), StateIn, StateOut) :-
    (   mi_call(A, StateIn, StateOut)
    ;   mi_call(B, StateIn, StateOut)
    ).

%% mi_call(\+(Goal), +StateIn, -StateOut)
mi_call(\+(Goal), StateIn, StateIn) :-
    \+ mi_call(Goal, StateIn, _).

%% mi_call(!, +StateIn, -StateOut)
%% Cut DOES need to actually cut
mi_call(!, StateIn, StateIn) :-
    !.

%% mi_call(true, +StateIn, -StateOut)
mi_call(true, StateIn, StateIn).

%% mi_call(fail, +StateIn, -StateOut)
mi_call(fail, _StateIn, _StateOut) :-
    fail.

%% mi_call(false, +StateIn, -StateOut)
mi_call(false, _StateIn, _StateOut) :-
    fail.

%% mi_call(assertz(Head), +StateIn, -StateOut)
%% mi_call(asserta(Head), +StateIn, -StateOut)
%% mi_call(retract(Head), +StateIn, -StateOut)
%% mi_call(retractall(Head), +StateIn, -StateOut)
%% Database predicates must operate on the session module so DML-side facts
%% (draft/4, claim/3, ...) are asserted/retracted in the right module.
%% repl_reserved_clause(+Term)
%% True when Term defines a reserved predicate. directives and the legitimate
%% repl_delete harness form are exempt.
repl_reserved_clause((:- _)) :-
    !,
    fail.
repl_reserved_clause(repl_delete(_)) :-
    !,
    fail.
repl_reserved_clause((tool(ToolHead, _) :- _)) :-
    !,
    functor(ToolHead, Name, _),
    repl_reserved_name(Name).
repl_reserved_clause((tool(ToolHead) :- _)) :-
    !,
    functor(ToolHead, Name, _),
    repl_reserved_name(Name).
repl_reserved_clause((Head :- _)) :-
    functor(Head, Name, _),
    repl_protected_name(Name).
repl_reserved_clause(Head) :-
    functor(Head, Name, _),
    repl_protected_name(Name).

mi_call(assertz(Head), StateIn, StateIn) :-
    get_session_id(SessionId),
    guard_not_frozen(SessionId),
    guard_reserved_clause(Head),
    assertz(SessionId:Head).

mi_call(asserta(Head), StateIn, StateIn) :-
    get_session_id(SessionId),
    guard_not_frozen(SessionId),
    guard_reserved_clause(Head),
    asserta(SessionId:Head).

guard_reserved_clause(Head) :-
    (   repl_reserved_clause(Head)
    ->  throw(error(permission_error(modify, reserved_name, Head),
                context(mi_call/3, 'cannot define a reserved predicate')))
    ;   true
    ).

mi_call(retract(Head), StateIn, StateIn) :-
    get_session_id(SessionId),
    guard_not_frozen(SessionId),
    retract(SessionId:Head).

mi_call(retractall(Head), StateIn, StateIn) :-
    get_session_id(SessionId),
    guard_not_frozen(SessionId),
    retractall(SessionId:Head).

%% ============================================================
%% List Predicates - Interpreted for proper state threading
%% ============================================================

%% mi_call(member(X, List), +StateIn, -StateOut)
%% List membership - allows backtracking through list elements
mi_call(member(X, List), StateIn, StateIn) :-
    member(X, List).

%% mi_call(append(A, B, C), +StateIn, -StateOut)
mi_call(append(A, B, C), StateIn, StateIn) :-
    append(A, B, C).

%% mi_call(length(List, Len), +StateIn, -StateOut)
mi_call(length(List, Len), StateIn, StateIn) :-
    length(List, Len).

%% mi_call(nth0(Index, List, Elem), +StateIn, -StateOut)
mi_call(nth0(Index, List, Elem), StateIn, StateIn) :-
    nth0(Index, List, Elem).

%% mi_call(nth1(Index, List, Elem), +StateIn, -StateOut)
mi_call(nth1(Index, List, Elem), StateIn, StateIn) :-
    nth1(Index, List, Elem).

%% mi_call(last(List, Elem), +StateIn, -StateOut)
mi_call(last(List, Elem), StateIn, StateIn) :-
    last(List, Elem).

%% mi_call(reverse(List, Reversed), +StateIn, -StateOut)
mi_call(reverse(List, Reversed), StateIn, StateIn) :-
    reverse(List, Reversed).

%% mi_call(sort(List, Sorted), +StateIn, -StateOut)
mi_call(sort(List, Sorted), StateIn, StateIn) :-
    sort(List, Sorted).

%% mi_call(msort(List, Sorted), +StateIn, -StateOut)
mi_call(msort(List, Sorted), StateIn, StateIn) :-
    msort(List, Sorted).

%% ============================================================
%% Meta-Predicates - Interpreted with proper state threading
%% ============================================================

%% mi_call(include(Goal, List, Included), +StateIn, -StateOut)
%% Filter list keeping elements where Goal succeeds
mi_call(include(Goal, List, Included), StateIn, StateOut) :-
    mi_include(Goal, List, Included, StateIn, StateOut).

mi_include(_, [], [], State, State).
mi_include(Goal, [H|T], Result, StateIn, StateOut) :-
    copy_term(Goal, GoalCopy),
    GoalCopy =.. GoalList,
    append(GoalList, [H], GoalWithArg),
    TestGoal =.. GoalWithArg,
    (   mi_call(TestGoal, StateIn, State1)
    ->  Result = [H|Rest],
        mi_include(Goal, T, Rest, State1, StateOut)
    ;   mi_include(Goal, T, Result, StateIn, StateOut)
    ).

%% mi_call(exclude(Goal, List, Excluded), +StateIn, -StateOut)
%% Filter list removing elements where Goal succeeds
mi_call(exclude(Goal, List, Excluded), StateIn, StateOut) :-
    mi_exclude(Goal, List, Excluded, StateIn, StateOut).

mi_exclude(_, [], [], State, State).
mi_exclude(Goal, [H|T], Result, StateIn, StateOut) :-
    copy_term(Goal, GoalCopy),
    GoalCopy =.. GoalList,
    append(GoalList, [H], GoalWithArg),
    TestGoal =.. GoalWithArg,
    (   mi_call(TestGoal, StateIn, State1)
    ->  mi_exclude(Goal, T, Result, State1, StateOut)
    ;   Result = [H|Rest],
        mi_exclude(Goal, T, Rest, StateIn, StateOut)
    ).

%% mi_call(partition(Goal, List, Included, Excluded), +StateIn, -StateOut)
%% Partition list into elements where Goal succeeds and fails
mi_call(partition(Goal, List, Included, Excluded), StateIn, StateOut) :-
    mi_partition(Goal, List, Included, Excluded, StateIn, StateOut).

mi_partition(_, [], [], [], State, State).
mi_partition(Goal, [H|T], Inc, Exc, StateIn, StateOut) :-
    copy_term(Goal, GoalCopy),
    GoalCopy =.. GoalList,
    append(GoalList, [H], GoalWithArg),
    TestGoal =.. GoalWithArg,
    (   mi_call(TestGoal, StateIn, State1)
    ->  Inc = [H|IncRest],
        mi_partition(Goal, T, IncRest, Exc, State1, StateOut)
    ;   Exc = [H|ExcRest],
        mi_partition(Goal, T, Inc, ExcRest, StateIn, StateOut)
    ).

%% mi_call(forall(Cond, Action), +StateIn, -StateOut)
%% Succeed if for all solutions of Cond, Action succeeds
%% Note: State changes in Action are NOT preserved (bagof semantics)
mi_call(forall(Cond, Action), StateIn, StateIn) :-
    \+ (mi_call(Cond, StateIn, State1), \+ mi_call(Action, State1, _)).

%% mi_call(findall(Template, Goal, List), +StateIn, -StateOut)
%% Collect all solutions - state changes are NOT preserved
mi_call(findall(Template, Goal, List), StateIn, StateIn) :-
    findall(Template, mi_call(Goal, StateIn, _), List).

%% mi_call(bagof(Template, Goal, List), +StateIn, -StateOut)
mi_call(bagof(Template, Goal, List), StateIn, StateIn) :-
    bagof(Template, mi_call(Goal, StateIn, _), List).

%% mi_call(setof(Template, Goal, List), +StateIn, -StateOut)
mi_call(setof(Template, Goal, List), StateIn, StateIn) :-
    setof(Template, mi_call(Goal, StateIn, _), List).

%% mi_call(aggregate_all(Template, Goal, Result), +StateIn, -StateOut)
mi_call(aggregate_all(Template, Goal, Result), StateIn, StateIn) :-
    aggregate_all(Template, mi_call(Goal, StateIn, _), Result).

%% mi_call(maplist(Goal, List), +StateIn, -StateOut)
%% Apply Goal to each element (Goal/1)
mi_call(maplist(Goal, List), StateIn, StateOut) :-
    mi_maplist1(Goal, List, StateIn, StateOut).

mi_maplist1(_, [], State, State).
mi_maplist1(Goal, [H|T], StateIn, StateOut) :-
    copy_term(Goal, GoalCopy),
    GoalCopy =.. GoalList,
    append(GoalList, [H], GoalWithArg),
    CallGoal =.. GoalWithArg,
    mi_call(CallGoal, StateIn, State1),
    mi_maplist1(Goal, T, State1, StateOut).

%% mi_call(maplist(Goal, List1, List2), +StateIn, -StateOut)
%% Apply Goal to pairs of elements (Goal/2)
mi_call(maplist(Goal, List1, List2), StateIn, StateOut) :-
    mi_maplist2(Goal, List1, List2, StateIn, StateOut).

mi_maplist2(_, [], [], State, State).
mi_maplist2(Goal, [H1|T1], [H2|T2], StateIn, StateOut) :-
    copy_term(Goal, GoalCopy),
    GoalCopy =.. GoalList,
    append(GoalList, [H1, H2], GoalWithArgs),
    CallGoal =.. GoalWithArgs,
    mi_call(CallGoal, StateIn, State1),
    mi_maplist2(Goal, T1, T2, State1, StateOut).

%% mi_call(maplist(Goal, L1, L2, L3), +StateIn, -StateOut)
%% Apply Goal to triples of elements (Goal/3)
mi_call(maplist(Goal, L1, L2, L3), StateIn, StateOut) :-
    mi_maplist3(Goal, L1, L2, L3, StateIn, StateOut).

mi_maplist3(_, [], [], [], State, State).
mi_maplist3(Goal, [H1|T1], [H2|T2], [H3|T3], StateIn, StateOut) :-
    copy_term(Goal, GoalCopy),
    GoalCopy =.. GoalList,
    append(GoalList, [H1, H2, H3], GoalWithArgs),
    CallGoal =.. GoalWithArgs,
    mi_call(CallGoal, StateIn, State1),
    mi_maplist3(Goal, T1, T2, T3, State1, StateOut).

%% mi_call(foldl(Goal, List, V0, V), +StateIn, -StateOut)
%% Left fold over list
mi_call(foldl(Goal, List, V0, V), StateIn, StateOut) :-
    mi_foldl(Goal, List, V0, V, StateIn, StateOut).

mi_foldl(_, [], V, V, State, State).
mi_foldl(Goal, [H|T], V0, V, StateIn, StateOut) :-
    copy_term(Goal, GoalCopy),
    GoalCopy =.. GoalList,
    append(GoalList, [H, V0, V1], GoalWithArgs),
    CallGoal =.. GoalWithArgs,
    mi_call(CallGoal, StateIn, State1),
    mi_foldl(Goal, T, V1, V, State1, StateOut).

%% ============================================================
%% Workspace predicates (one definition, every surface)
%% ============================================================
%%
%% These are **real clauses in the session module**, not only meta-interpreter
%% handlers. `read_file/2` in particular has no other definition in this build:
%% without these clauses it exists only as an `mi_call/3` body, so it works from
%% DML but fails from a host query. Installing them makes a host query, `execute`,
%% `/goal` and a nested `task/N` resolve the same predicate.
%%
%% The file-stream primitives live in `system:` (NOT `readutil`, which has no
%% `read_file_to_string/3`). They are module-qualified so a wrapper cannot
%% recurse into itself.
%%
%% Deliberately not here: `open/3`, `close/1`, `read_string/3`, `write/2`. They
%% are imported builtins in the session module and SWI refuses to shadow an
%% imported private predicate, so a workspace-resolving version cannot be
%% installed. `read_file/2` and `write_file/2` are the whole-file API instead.
%%
%% `workspace_predicate_name/1` is the single source of truth; it also keeps the
%% set out of `listing` / `export` / history.

workspace_predicate_name(read_file).
workspace_predicate_name(read_file_to_string).
workspace_predicate_name(write_file).
workspace_predicate_name(append_file).
workspace_predicate_name(exists_file).
workspace_predicate_name(exists_directory).
workspace_predicate_name(directory_files).
workspace_predicate_name(make_directory).
workspace_predicate_name(make_directory_path).
workspace_predicate_name(delete_file).
workspace_predicate_name(delete_directory).
workspace_predicate_name(size_file).
workspace_predicate_name(time_file).
workspace_predicate_name(dict_field).
workspace_predicate_name(dict_field_key).
workspace_predicate_name(json_get).
workspace_predicate_name(json_gets).
workspace_predicate_name(json_put).
workspace_predicate_name(json_text).
workspace_predicate_name(schema_fill).
workspace_predicate_name(pattern).

%% dict_field(+Dict, +Key, -Value) and dict_field(+Dict, +Key, +Default, -Value)
%%
%% Look up a field of a model-produced dict. SWI dicts have *atom* keys and
%% `get_dict/3` is strict: `get_dict("affiliation", D)` raises
%% `type_error(dict-key, affiliation)` rather than failing, and the model varies
%% the spelling (`Name` / `name` / `full_name`). This matches on a normalised
%% atom — any of atom/string, any case, `_`/`-`/space ignored — so
%% `dict_field(D, "affiliation", A)` finds the `affiliation` key.
dict_field(Dict, Key, Value) :-
    is_dict(Dict),
    dict_field_key(Key, Wanted),
    dict_pairs(Dict, _, Pairs),
    member(K-V, Pairs),
    dict_field_key(K, Wanted),
    Value = V.

dict_field(Dict, Key, Default, Value) :-
    (   dict_field(Dict, Key, Found)
    ->  Value = Found
    ;   Value = Default
    ).

%% dict_field_key(+Key, -Normalised): lowercase atom with separators removed.
dict_field_key(Key, Normalised) :-
    (   atom(Key) -> atom_string(Key, Text) ; Text = Key ),
    string_lower(Text, Lower),
    split_string(Lower, " _-", "", Parts),
    atomic_list_concat(Parts, '', Normalised).

%% json_get/3,4, json_gets/3, json_put/4, json_text/2
%%
%% Path access into a model-produced structure, so a nested lookup is one call
%% rather than a chain of dict_field/3. A path is a list of keys and indices, or a
%% string/atom with `.` separators (`"results.0.authors.1.name"`), which is how a
%% JSON path is normally written.
%%
%% A missing path **fails** — it does not throw and does not yield "" — so absence
%% is visible at the call site; json_get/4 supplies a default instead. Key matching
%% is the same tolerant match as dict_field/3 (atom or string, any case,
%% `_`/`-`/space ignored).
json_path_segments(Path, Segments) :-
    (   is_list(Path)
    ->  Segments = Path
    ;   (   atom(Path) -> atom_string(Path, Text) ; Text = Path ),
        split_string(Text, ".", "", RawParts),
        exclude(==(""), RawParts, Parts),
        maplist(json_path_segment, Parts, Segments)
    ).

json_path_segment("*", star) :- !.
json_path_segment(Part, Index) :-
    catch(number_string(N, Part), _, fail),
    integer(N),
    !,
    Index = N.
json_path_segment(Part, Part).

%% json_walk(+Value, +Segments, -Found)
json_walk(Value, [], Value) :- !.
json_walk(Doc, [Index|Rest], Found) :-
    integer(Index),
    is_list(Doc),
    !,
    nth0(Index, Doc, Next),
    json_walk(Next, Rest, Found).
json_walk(Doc, [Key|Rest], Found) :-
    is_dict(Doc),
    dict_field_key(Key, Wanted),
    dict_pairs(Doc, _, Pairs),
    member(K-V, Pairs),
    dict_field_key(K, Wanted),
    json_walk(V, Rest, Found).

json_get(Doc, Path, Value) :-
    json_path_segments(Path, Segments),
    \+ member(star, Segments),
    json_walk(Doc, Segments, Value).

json_get(Doc, Path, Default, Value) :-
    (   json_get(Doc, Path, Found)
    ->  Value = Found
    ;   Value = Default
    ).

%% json_gets/3: `*` maps over a list, collecting the remainder for every element.
%% Nested `*`s flatten, so "results.*.authors.*.name" is a flat list of names.
json_gets(Doc, Path, Values) :-
    json_path_segments(Path, Segments),
    json_collect(Doc, Segments, Values).

json_collect(Value, [], [Value]) :- !.
json_collect(Doc, [star|Rest], Values) :-
    is_list(Doc),
    findall(Collected,
        ( member(Item, Doc), json_collect(Item, Rest, Collected) ),
        Nested),
    append(Nested, Values).
json_collect(Doc, [Seg|Rest], Values) :-
    json_walk_1(Doc, Seg, Next),
    json_collect(Next, Rest, Values).

%% One step of a walk, for the collector (where `star` is handled by the caller).
json_walk_1(Doc, Index, Value) :- integer(Index), is_list(Doc), !, nth0(Index, Doc, Value).
json_walk_1(Doc, Key, Value) :-
    is_dict(Doc),
    dict_field_key(Key, Wanted),
    dict_pairs(Doc, _, Pairs),
    member(K-V, Pairs),
    dict_field_key(K, Wanted),
    Value = V.

%% json_put/4: set a key path. Lists are read-only here — an index path is
%% rejected, because building output is about setting fields, not splicing lists.
json_put(Doc, Path, Value, Updated) :-
    json_path_segments(Path, Segments),
    Segments \== [],
    \+ member(star, Segments),
    % NB: `\+ ( member(S, Segments), integer(S) )`. Writing this as
    % `\+ member(S, Segments), integer(S)` binds S inside the negation, so the
    % whole guard fails for any non-empty path.
    \+ ( member(Seg, Segments), integer(Seg) ),
    json_put_walk(Doc, Segments, Value, Updated).

json_put_walk(_Doc, [], Value, Value) :- !.
json_put_walk(Doc, [Key|Rest], Value, Updated) :-
    is_dict(Doc),
    (   json_walk_1(Doc, Key, Old)
    ->  json_put_walk(Old, Rest, Value, NewValue)
    ;   json_put_walk(_{}, Rest, Value, NewValue)
    ),
    dict_field_key(Key, Key0),
    put_dict(Key0, Doc, NewValue, Updated).

%% json_text(+Dict, -Text): compact JSON. For layout or streaming use
%% json_write_dict/3 from library(http/json).
json_text(Dict, Text) :- atom_json_dict(Text, Dict, []).

%% pattern(+Template, ?Text, ?Bindings)
%%
%% One syntax for building a string and for parsing it. Literal text matches
%% exactly, and `$Name` marks a slot; the direction follows from what is bound:
%%
%%   pattern("Name: $Name, age: $Age", Text, [name="Ada", age=36])   % build
%%   pattern("Name: $Name, age: $Age", "Name: Ada, age: 36", B)      % parse
%%   pattern("DOI: $Doi:doi", Text, [doi=D])                         % typed slot
%%
%% `$` is the marker rather than `{…}` so a template can contain JSON or any other
%% brace-bearing text verbatim, and so it never collides with the `{Var}`
%% interpolation used in task/N descriptions. `$$` is a literal dollar.
%%
%% A template must be anchored by literal text: two slots separated only by nothing
%% (`"$A$B"`) is rejected, rather than resolved arbitrarily. On parse a slot
%% consumes up to the next literal, so the literals decide the split.
pattern(Template, Text, Bindings) :-
    pattern_tokens(Template, Tokens),
    (   var(Text)
    ->  pattern_build(Tokens, Bindings, Text)
    ;   pattern_parse(Tokens, Text, Bindings)
    ).

%% pattern_tokens(+Template, -Tokens): [lit(CharList)|slot(NameChars, TypeChars)]
pattern_tokens(Template, Tokens) :-
    (   atom(Template) -> atom_string(Template, Text) ; Text = Template ),
    string_chars(Text, Chars),
    pattern_scan(Chars, [], Tokens),
    pattern_check_anchored(Tokens).

pattern_scan([], Acc, [lit(Text)]) :-
    !,
    reverse(Acc, Text).
pattern_scan(['$', '$' | Rest], Acc, Tokens) :-
    !,
    pattern_scan(Rest, ['$' | Acc], Tokens).
pattern_scan(['$' | Rest], Acc, Tokens) :-
    !,
    pattern_take_name(Rest, NameChars, Rest1),
    NameChars \== [],
    atom_chars(Name, NameChars),
    (   Rest1 = [':' | Rest2],
        pattern_take_name(Rest2, TypeChars, Rest3),
        TypeChars \== []
    ->  atom_chars(Type, TypeChars)
    ;   Rest3 = Rest1, Type = none
    ),
    reverse(Acc, Text),
    Tokens = [lit(Text), slot(Name, Type) | More],
    pattern_scan(Rest3, [], More).
pattern_scan([C | Rest], Acc, Tokens) :-
    pattern_scan(Rest, [C | Acc], Tokens).

%% A slot name is a run of characters that are not a template delimiter. Scanning by
%% delimiter rather than by `char_type(C, alnum)` avoids depending on a ctypes
%% predicate that is not guaranteed in this build.
pattern_take_name([C | Rest], [C | More], Left) :-
    \+ pattern_delimiter(C),
    !,
    pattern_take_name(Rest, More, Left).
pattern_take_name(Rest, [], Rest).

pattern_delimiter(' ').  pattern_delimiter('\t'). pattern_delimiter('\n').
pattern_delimiter(':').  pattern_delimiter(',').  pattern_delimiter('.').
pattern_delimiter(')').  pattern_delimiter(']').  pattern_delimiter('}').
pattern_delimiter('$').  pattern_delimiter('-').  pattern_delimiter('/').
pattern_delimiter('=').  pattern_delimiter('\'').

%% Two slots with nothing between them cannot be told apart on parse.
pattern_check_anchored(Tokens) :-
    pattern_adjacent_slots(Tokens, A, B),
    !,
    format(atom(Msg),
        'pattern: slots "~w" and "~w" are adjacent with no literal between them, so the split is ambiguous',
        [A, B]),
    throw(error(domain_error(pattern_unanchored, Msg), pattern_tokens/2)).
pattern_check_anchored(_).

pattern_adjacent_slots([slot(A, _), lit([]), slot(B, _) | _], A, B) :- !.
pattern_adjacent_slots([_ | Rest], A, B) :- pattern_adjacent_slots(Rest, A, B).

%% pattern_build(+Tokens, +Bindings, -Text)
pattern_build(Tokens, Bindings, Text) :-
    findall(Chars,
        (   member(Token, Tokens),
            pattern_build_token(Token, Bindings, Chars)
        ),
        Parts),
    append(Parts, All),
    string_chars(Text, All).

pattern_build_token(lit(Chars), _, Chars) :- !.
pattern_build_token(slot(Name, Type), Bindings, Chars) :-
    (   member(Name=Value, Bindings)
    ->  true
    ;   member(Key=Value, Bindings), pattern_name(Key, Name)
    ->  true
    ;   format(atom(Msg), 'pattern: no value bound for slot "~w"', [Name]),
        throw(error(uninstantiation_error(Msg), pattern/3))
    ),
    pattern_render(Type, Value, Chars).

pattern_render(none, Value, Chars) :- !, pattern_text_chars(Value, Chars).
pattern_render(Type, Value, Chars) :-
    (   memberchk(Type, [string, text])
    ->  pattern_text_chars(Value, Chars)
    ;   memberchk(Type, [int, integer, number])
    ->  (   number(Value) -> format(string(S), "~w", [Value]) ; S = Value ),
        string_chars(S, Chars)
    ;   format(atom(Msg), 'pattern: unknown slot type "~w"', [Type]),
        throw(error(domain_error(pattern_type, Msg), pattern/3))
    ).

%% Always a char LIST. Returning the string itself here silently corrupted the
%% build: the parts list then held a string among char lists, and the final
%% string_chars/2 failed on a mixed list.
pattern_text_chars(Value, Chars) :-
    (   string(Value) -> string_chars(Value, Chars)
    ;   atom(Value) -> atom_chars(Value, Chars)
    ;   number(Value) -> format(string(S), "~w", [Value]), string_chars(S, Chars)
    ;   format(atom(Msg), 'pattern: cannot render ~w as text', [Value]),
        throw(error(type_error(text, Msg), pattern/3))
    ).

%% pattern_parse(+Tokens, +Text, -Bindings)
pattern_parse(Tokens, Text, Bindings) :-
    % Always a char list: a string here would be one element and the literal
    % matching below would never line up.
    (   string(Text) -> string_chars(Text, Chars)
    ;   atom(Text) -> atom_string(Text, S), string_chars(S, Chars)
    ;   Chars = Text
    ),
    is_list(Chars),
    pattern_match(Tokens, Chars, Bindings).

pattern_match([], [], []) :- !.
pattern_match([lit(Lit)], Rest, []) :-
    !,
    append(Lit, [], Rest).
pattern_match([lit(Lit), slot(Name, Type) | More], Chars, [Name=Value | Bindings]) :-
    append(Lit, Rest0, Chars),
    !,
    (   More = [lit(Next) | _], Next \== []
    ->  append(Taken, After, Rest0),
        append(Next, _, After)
    ;   Taken = Rest0,
        After = []
    ),
    pattern_capture(Type, Taken, Value),
    pattern_match(More, After, Bindings).

pattern_capture(none, Chars, Value) :- !, string_chars(Value, Chars).
pattern_capture(Type, Chars, Value) :-
    (   memberchk(Type, [string, text])
    ->  string_chars(Value, Chars)
    ;   memberchk(Type, [int, integer])
    ->  (   catch(number_codes(N, Chars), _, fail), integer(N)
        ->  Value = N
        ;   format(atom(Msg), 'pattern: "~s" is not an integer', [Chars]),
            throw(error(syntax_error(Msg), pattern/3))
        )
    ;   memberchk(Type, [number])
    ->  (   catch(number_codes(N, Chars), _, fail)
        ->  Value = N
        ;   format(atom(Msg), 'pattern: "~s" is not a number', [Chars]),
            throw(error(syntax_error(Msg), pattern/3))
        )
    ;   format(atom(Msg), 'pattern: unknown slot type "~w"', [Type]),
        throw(error(domain_error(pattern_type, Msg), pattern/3))
    ).

%% pattern_name(+Key, +Name): compare a template slot name with a binding key the
%% way dict_field does — atom or string, any case, separators ignored.
pattern_name(Key, Name) :-
    pattern_normalise(Key, A),
    pattern_normalise(Name, B),
    A == B.

pattern_normalise(Value, Normalised) :-
    (   atom(Value) -> atom_string(Value, Text) ; Text = Value ),
    string_lower(Text, Lower),
    split_string(Lower, " _-", "", Parts),
    atomic_list_concat(Parts, '', Normalised).

%% repl_inject_workspace_predicates(+SessionId)
%% Idempotent: assertz/1 the same clauses twice is harmless (a duplicate clause
%% yields the same solution once? no - it yields twice), so clear first.
repl_inject_workspace_predicates(SessionId) :-
    forall(( workspace_predicate_name(Name), between(0, 5, Arity) ),
        catch(abolish(SessionId:Name/Arity), _, true)),
    forall(workspace_predicate_clause(SessionId, Clause),
        catch(assertz(Clause), _, true)).

workspace_predicate_clause(S, (S:read_file(F, C) :-
    system:read_file_to_string(F, C, []))).
workspace_predicate_clause(S, (S:read_file_to_string(F, C, O) :-
    system:read_file_to_string(F, C, O))).
workspace_predicate_clause(S, (S:write_file(F, C) :-
    system:open(F, write, St), system:write(St, C), system:close(St))).
workspace_predicate_clause(S, (S:append_file(F, C) :-
    system:open(F, append, St), system:write(St, C), system:close(St))).
workspace_predicate_clause(S, (S:exists_file(F) :-
    system:exists_file(F))).
workspace_predicate_clause(S, (S:exists_directory(D) :-
    system:exists_directory(D))).
workspace_predicate_clause(S, (S:directory_files(D, L) :-
    system:directory_files(D, L))).
workspace_predicate_clause(S, (S:make_directory(D) :-
    system:make_directory(D))).
workspace_predicate_clause(S, (S:make_directory_path(D) :-
    system:make_directory_path(D))).
workspace_predicate_clause(S, (S:delete_file(F) :-
    system:delete_file(F))).
workspace_predicate_clause(S, (S:delete_directory(D) :-
    system:delete_directory(D))).
workspace_predicate_clause(S, (S:size_file(F, Sz) :-
    system:size_file(F, Sz))).
workspace_predicate_clause(S, (S:dict_field(D, K, V) :- deepclause_mi:dict_field(D, K, V))).
workspace_predicate_clause(S, (S:dict_field(D, K, Def, V) :- deepclause_mi:dict_field(D, K, Def, V))).
workspace_predicate_clause(S, (S:dict_field_key(K, N) :- deepclause_mi:dict_field_key(K, N))).
workspace_predicate_clause(S, (S:json_get(D, P, V) :- deepclause_mi:json_get(D, P, V))).
workspace_predicate_clause(S, (S:json_get(D, P, Def, V) :- deepclause_mi:json_get(D, P, Def, V))).
workspace_predicate_clause(S, (S:json_gets(D, P, Vs) :- deepclause_mi:json_gets(D, P, Vs))).
workspace_predicate_clause(S, (S:json_put(D, P, V, D2) :- deepclause_mi:json_put(D, P, V, D2))).
workspace_predicate_clause(S, (S:json_text(D, T) :- deepclause_mi:json_text(D, T))).
workspace_predicate_clause(S, (S:schema_fill(N, V, F) :-
    deepclause_mi:schema_fields_of(S, N, Fields), deepclause_mi:schema_fill_fields(Fields, V, F))).
workspace_predicate_clause(S, (S:pattern(T, Text, B) :- deepclause_mi:pattern(T, Text, B))).

workspace_predicate_clause(S, (S:time_file(F, T) :-
    system:time_file(F, T))).

%% ============================================================
%% File I/O - paths resolve against /workspace (see the note above)
%% ============================================================

%% File I/O paths
%% `/workspace` is the host project, mounted by the WASM filesystem (NODEFS);
%% its working directory is set to /workspace, so a relative path and a
%% `/workspace/...` path name the same file. Every other absolute path is the
%% ephemeral in-memory VM filesystem, not the host. There is no rewriting
%% layer any more: the mount is the boundary.

%% mi_call(consult(File), +StateIn, -StateOut)
%% Load Prolog clauses from a file into the current session module
%% This is a DML-specific implementation that reads from /workspace
mi_call(consult(File), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    guard_not_frozen(SessionId),
    system:read_file_to_string(File, Code, []),
    consult_string(Code, SessionId).

mi_call(use_module(Module), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    process_use_module_directive(Module, SessionId).

mi_call(use_module(Module, Exports), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    process_use_module_directive(Module, Exports, SessionId).

%% mi_call([File], +StateIn, -StateOut)
%% Alternative consult syntax: [file]
mi_call([File], StateIn, StateOut) :-
    !,
    mi_call(consult(File), StateIn, StateOut).

%% mi_call([File|Files], +StateIn, -StateOut)
%% Consult multiple files: [file1, file2, ...]
mi_call([File|Files], StateIn, StateOut) :-
    !,
    mi_call(consult(File), StateIn, State1),
    mi_call(Files, State1, StateOut).

%% consult_string(+Code, +SessionId)
%% Parse Prolog code from a string and assert clauses into the session module
consult_string(Code, SessionId) :-
    open_string(Code, Stream),
    consult_clauses(Stream, SessionId),
    close(Stream).

%% consult_clauses(+Stream, +SessionId)
%% Read and assert all clauses from a stream
consult_clauses(Stream, SessionId) :-
    read_term(Stream, Term, [module(SessionId), variable_names(Bindings)]),
    (   Term == end_of_file
    ->  true
    ;   preprocess_loaded_term(Term, Bindings, PreparedTerms),
        forall(member(PreparedTerm, PreparedTerms), consult_process_term(PreparedTerm, SessionId)),
        consult_clauses(Stream, SessionId)
    ).

%% consult_process_term(+Term, +SessionId)
%% Process a single term read during consult. Reserved-name definitions are
%% refused so consulted files cannot shadow the REPL control surface.
consult_process_term(Term, _SessionId) :-
    repl_reserved_clause(Term),
    !,
    throw(error(permission_error(modify, reserved_name, Term),
                context(consult_process_term/2, 'reserved predicate'))).

consult_process_term(:-(Directive), SessionId) :-
    % Handle directives - route through mi_call so custom predicates like consult/1 work
    !,
    catch(
        (   is_mi_special_predicate(Directive)
        ->  mi_call(Directive, [], _)  % Use MI for special predicates
        ;   call(SessionId:Directive)   % Regular call for others
        ),
        Error,
        format(user_error, "Warning: directive failed: ~w~n", [Error])
    ).

%% Handle tool/2 with description: tool(Head, Description) :- Body
consult_process_term((tool(ToolHead, Description) :- Body), SessionId) :-
    !,
    extract_tool_schema(ToolHead, Description, ToolName, Schema),
    assertz(session_user_tools(SessionId, ToolName)),
    assertz(session_user_tool_schema(SessionId, ToolName, Schema)),
    format_tool_source(ToolHead, Description, Body, SourceCode),
    assertz(SessionId:tool_source(ToolName, SourceCode)),
    assertz(SessionId:(tool(ToolHead) :- Body)).

%% Handle tool/1 without description: tool(Head) :- Body
consult_process_term((tool(ToolHead) :- Body), SessionId) :-
    !,
    extract_tool_schema(ToolHead, none, ToolName, Schema),
    assertz(session_user_tools(SessionId, ToolName)),
    assertz(session_user_tool_schema(SessionId, ToolName, Schema)),
    format_tool_source(ToolHead, none, Body, SourceCode),
    assertz(SessionId:tool_source(ToolName, SourceCode)),
    assertz(SessionId:(tool(ToolHead) :- Body)).

consult_process_term((Head :- Body), SessionId) :-
    % Expand dict dot-notation, then assert
    !,
    (catch(expand_dict_dots(Body, ExpandedBody), _, fail) -> true ; ExpandedBody = Body),
    assertz(SessionId:(Head :- ExpandedBody)).

consult_process_term(Head, SessionId) :-
    % Assert a fact
    assertz(SessionId:Head).

%% mi_call(read_file_to_string(File, Content, Options), +StateIn, -StateOut)
mi_call(read_file_to_string(File, Content, Options), StateIn, StateIn) :-
    !,
    system:read_file_to_string(File, Content, Options).

%% mi_call(read_file(File, Content), +StateIn, -StateOut)
%% Simplified version without options
mi_call(read_file(File, Content), StateIn, StateIn) :-
    !,
    system:read_file_to_string(File, Content, []).

%% mi_call(write_file(File, Content), +StateIn, -StateOut)
%% Write string content to a file
mi_call(write_file(File, Content), StateIn, StateIn) :-
    !,
    open(File, write, Stream),
    write(Stream, Content),
    close(Stream).

%% mi_call(append_file(File, Content), +StateIn, -StateOut)
%% Append string content to a file
mi_call(append_file(File, Content), StateIn, StateIn) :-
    !,
    open(File, append, Stream),
    write(Stream, Content),
    close(Stream).

%% mi_call(open(File, Mode, Stream), +StateIn, -StateOut)
%% Open a file for reading/writing
mi_call(open(File, Mode, Stream), StateIn, StateIn) :-
    !,
    open(File, Mode, Stream).

%% mi_call(close(Stream), +StateIn, -StateOut)
mi_call(close(Stream), StateIn, StateIn) :-
    !,
    close(Stream).

%% mi_call(write(Stream, Content), +StateIn, -StateOut)
mi_call(write(Stream, Content), StateIn, StateIn) :-
    !,
    write(Stream, Content).

%% mi_call(exists_file(File), +StateIn, -StateOut)
mi_call(exists_file(File), StateIn, StateIn) :-
    !,
    exists_file(File).

%% mi_call(exists_directory(Dir), +StateIn, -StateOut)
mi_call(exists_directory(Dir), StateIn, StateIn) :-
    !,
    exists_directory(Dir).

%% mi_call(directory_files(Dir, Files), +StateIn, -StateOut)
mi_call(directory_files(Dir, Files), StateIn, StateIn) :-
    !,
    directory_files(Dir, Files).

%% mi_call(make_directory(Dir), +StateIn, -StateOut)
mi_call(make_directory(Dir), StateIn, StateIn) :-
    !,
    make_directory(Dir).

%% mi_call(make_directory_path(Dir), +StateIn, -StateOut)
mi_call(make_directory_path(Dir), StateIn, StateIn) :-
    !,
    make_directory_path(Dir).

%% mi_call(delete_file(File), +StateIn, -StateOut)
mi_call(delete_file(File), StateIn, StateIn) :-
    !,
    delete_file(File).

%% mi_call(delete_directory(Dir), +StateIn, -StateOut)
mi_call(delete_directory(Dir), StateIn, StateIn) :-
    !,
    delete_directory(Dir).

%% mi_call(size_file(File, Size), +StateIn, -StateOut)
%% size_file/2 is a plain SWI builtin, so it would otherwise resolve `File`
%% against the WASM process cwd (not the workspace) and demand `/workspace/...`.
mi_call(size_file(File, Size), StateIn, StateIn) :-
    !,
    size_file(File, Size).

%% mi_call(time_file(File, Time), +StateIn, -StateOut)
%% Modifications time (seconds since epoch); same workspace-relative rule as
%% size_file/2.
mi_call(time_file(File, Time), StateIn, StateIn) :-
    !,
    time_file(File, Time).

%% ============================================================
%% Catch/Throw - STATE THREADED
%% ============================================================

%% mi_call(catch(Goal, Catcher, Recovery), +StateIn, -StateOut)
mi_call(catch(Goal, Catcher, Recovery), StateIn, StateOut) :-
    catch(
        mi_call(Goal, StateIn, StateOut),
        Catcher,
        mi_call(Recovery, StateIn, StateOut)
    ).

%% mi_call(throw(Error), +StateIn, -StateOut)
mi_call(throw(Error), _StateIn, _StateOut) :-
    !,
    throw(Error).

%% ============================================================
%% DCG Helpers
%% ============================================================

%% phrase/2 and phrase/3 must run in the session module so they can resolve
%% grammar predicates asserted from the current DML source.
mi_call(phrase(Grammar, Input), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    call(SessionId:phrase(Grammar, Input)).

mi_call(phrase(Grammar, Input, Rest), StateIn, StateIn) :-
    !,
    get_session_id(SessionId),
    call(SessionId:phrase(Grammar, Input, Rest)).

%% ============================================================
%% Module-Qualified Goals
%% ============================================================

%% mi_call(Module:Goal, +StateIn, -StateOut)
%% Module-qualified goals - handle special predicates and user-defined predicates

mi_call(Module:phrase(Grammar, Input), StateIn, StateIn) :-
    !,
    call(Module:phrase(Grammar, Input)).

mi_call(Module:phrase(Grammar, Input, Rest), StateIn, StateIn) :-
    !,
    call(Module:phrase(Grammar, Input, Rest)).

% First check if the unqualified Goal is a special MI predicate - if so, handle it directly
mi_call(_Module:Goal, StateIn, StateOut) :-
    is_mi_special_predicate(Goal),
    !,
    mi_call(Goal, StateIn, StateOut).

% For regular module-qualified goals, use clause/2 for backtracking
mi_call(Module:Goal, StateIn, StateOut) :-
    predicate_property(Module:Goal, defined),
    !,  % CUT: If predicate is defined, don't fallback to call/1
    % Now try clause/2 for rules, allowing backtracking through clauses
    clause(Module:Goal, Body),
    (   Body == true
    ->  StateOut = StateIn  % Fact - succeed with state unchanged
    ;   mi_call(Body, StateIn, StateOut)
    ).

mi_call(Module:Goal, StateIn, StateIn) :-
    % Fallback for built-ins only (predicate is NOT defined in module)
    call(Module:Goal).

%% ============================================================
%% Built-in and User-Defined Predicates
%% ============================================================

%% is_mi_special_predicate(+Goal)
%% Check if Goal is a special predicate handled by dedicated mi_call clauses
is_mi_special_predicate(answer(_)).
is_mi_special_predicate(output(_)).
is_mi_special_predicate(yield(_)).
is_mi_special_predicate(log(_)).
is_mi_special_predicate(system(_)).
is_mi_special_predicate(user(_)).  %% Add user message to context
is_mi_special_predicate(read_media(_,_)).
is_mi_special_predicate(attach_media(_)).
is_mi_special_predicate(save_image(_,_)).
is_mi_special_predicate(task(_)).
is_mi_special_predicate(task(_,_)).
is_mi_special_predicate(task(_,_,_)).
is_mi_special_predicate(task(_,_,_,_)).
is_mi_special_predicate(task(_,_,_,_,_)).
is_mi_special_predicate(task_named(_,_,_)).   %% Transformed form of task/N
is_mi_special_predicate(sample_token(_,_)).
is_mi_special_predicate(sample_token(_,_,_)).
is_mi_special_predicate(exec(_,_)).
is_mi_special_predicate(spawn(_,_)).
is_mi_special_predicate(spawn_opt(_,_,_)).
is_mi_special_predicate(self(_)).
is_mi_special_predicate(send(_,_)).
is_mi_special_predicate(receive(_)).
is_mi_special_predicate(receive(_,_)).
is_mi_special_predicate(receive_match(_,_,_)).
is_mi_special_predicate(exit(_)).
is_mi_special_predicate(exit(_,_)).
is_mi_special_predicate(process_info(_,_)).
is_mi_special_predicate(register(_,_)).
is_mi_special_predicate(whereis(_,_)).
is_mi_special_predicate(sleep(_)).
is_mi_special_predicate(make_ref(_)).
is_mi_special_predicate(now_ms(_)).
is_mi_special_predicate(serve(_,_)).
is_mi_special_predicate(service_cast(_,_)).
is_mi_special_predicate(service_call(_,_,_)).
is_mi_special_predicate(service_call(_,_,_,_)).
is_mi_special_predicate(swarm(_,_,_)).
is_mi_special_predicate(swarm(_,_,_,_)).
is_mi_special_predicate(pmap(_,_,_)).
is_mi_special_predicate(wait(_)).
is_mi_special_predicate(wait(_,_)).
is_mi_special_predicate(param(_,_)).
is_mi_special_predicate(param(_,_,_)).
%% Prompt predicates (like task but with fresh memory)
is_mi_special_predicate(prompt(_)).
is_mi_special_predicate(prompt(_,_)).
is_mi_special_predicate(prompt(_,_,_)).
is_mi_special_predicate(prompt(_,_,_,_)).
is_mi_special_predicate(prompt_named(_,_,_)). %% Transformed form of prompt/N
is_mi_special_predicate(llm(_,_)).
%% Tool scoping predicates
is_mi_special_predicate(with_tools(_,_)).
is_mi_special_predicate(with_fresh_memory(_)).
is_mi_special_predicate(without_tools(_,_)).
%% Reasoning effort scoping
is_mi_special_predicate(with_reasoning(_,_)).
is_mi_special_predicate(with_model(_,_)).
%% Recipe context scoping
is_mi_special_predicate(with_recipe(_,_)).
%% Semantic judgment predicates
is_mi_special_predicate(judge(_,_)).
is_mi_special_predicate(choose(_,_,_,_)).
is_mi_special_predicate(rate(_,_,_,_)).
is_mi_special_predicate(verify(_,_,_)).
is_mi_special_predicate(probability(_,_,_)).
is_mi_special_predicate(holds(_,_)).
is_mi_special_predicate(holds(_,_,_)).
is_mi_special_predicate(with_judgment(_,_)).
is_mi_special_predicate(require_judgment(_,_)).
%% Context stack management
is_mi_special_predicate(get_memory(_)).
is_mi_special_predicate(format(_,_)).
is_mi_special_predicate(writeln(_)).
is_mi_special_predicate(input(_,_)).
is_mi_special_predicate(call(_)).
is_mi_special_predicate(call(_,_)).
is_mi_special_predicate(call(_,_,_)).
is_mi_special_predicate(call(_,_,_,_)).
is_mi_special_predicate(once(_)).
is_mi_special_predicate(ignore(_)).
is_mi_special_predicate(not(_)).
is_mi_special_predicate(push_context).
is_mi_special_predicate(push_context(_)).
is_mi_special_predicate(pop_context).
is_mi_special_predicate(clear_memory).
%% Control flow
is_mi_special_predicate((_,_)).
is_mi_special_predicate((_;_)).
is_mi_special_predicate((_->_)).
is_mi_special_predicate((_->_;_)).
is_mi_special_predicate(\+(_)).
is_mi_special_predicate(catch(_,_,_)).
is_mi_special_predicate(throw(_)).
is_mi_special_predicate(!).
is_mi_special_predicate(true).
is_mi_special_predicate(fail).
is_mi_special_predicate(false).
is_mi_special_predicate(assertz(_)).
is_mi_special_predicate(asserta(_)).
is_mi_special_predicate(retract(_)).
is_mi_special_predicate(retractall(_)).
is_mi_special_predicate(_:_).
%% List predicates
is_mi_special_predicate(member(_,_)).
is_mi_special_predicate(append(_,_,_)).
is_mi_special_predicate(length(_,_)).
is_mi_special_predicate(nth0(_,_,_)).
is_mi_special_predicate(nth1(_,_,_)).
is_mi_special_predicate(last(_,_)).
is_mi_special_predicate(reverse(_,_)).
is_mi_special_predicate(sort(_,_)).
is_mi_special_predicate(msort(_,_)).
%% Meta-predicates
is_mi_special_predicate(include(_,_,_)).
is_mi_special_predicate(exclude(_,_,_)).
is_mi_special_predicate(partition(_,_,_,_)).
is_mi_special_predicate(forall(_,_)).
is_mi_special_predicate(findall(_,_,_)).
is_mi_special_predicate(bagof(_,_,_)).
is_mi_special_predicate(setof(_,_,_)).
is_mi_special_predicate(aggregate_all(_,_,_)).
is_mi_special_predicate(maplist(_,_)).
is_mi_special_predicate(maplist(_,_,_)).
is_mi_special_predicate(maplist(_,_,_,_)).
is_mi_special_predicate(foldl(_,_,_,_)).
%% DCG predicates
is_mi_special_predicate(phrase(_,_)).
is_mi_special_predicate(phrase(_,_,_)).
%% File I/O predicates
is_mi_special_predicate(read_file_to_string(_,_,_)).
is_mi_special_predicate(read_file(_,_)).
is_mi_special_predicate(write_file(_,_)).
is_mi_special_predicate(append_file(_,_)).
is_mi_special_predicate(open(_,_,_)).
is_mi_special_predicate(close(_)).
is_mi_special_predicate(read_string(_,_,_)).
is_mi_special_predicate(write(_,_)).
is_mi_special_predicate(exists_file(_)).
is_mi_special_predicate(size_file(_,_)).
is_mi_special_predicate(time_file(_,_)).
is_mi_special_predicate(exists_directory(_)).
is_mi_special_predicate(directory_files(_,_)).
is_mi_special_predicate(make_directory(_)).
is_mi_special_predicate(make_directory_path(_)).
is_mi_special_predicate(delete_file(_)).
is_mi_special_predicate(delete_directory(_)).
%% Consult and module predicates
is_mi_special_predicate(consult(_)).
is_mi_special_predicate(use_module(_)).
is_mi_special_predicate(use_module(_,_)).
is_mi_special_predicate([_]).
is_mi_special_predicate([_|_]).

%% mi_call(call(Goal), +StateIn, -StateOut)
%% Route call/1 through the meta-interpreter so backtracking and the
%% reserved-predicate guard apply (a bare call/repl_* would bypass them).
mi_call(call(Goal), StateIn, StateOut) :-
    guard_reserved_goal(Goal, StateIn),
    !,
    mi_call(Goal, StateIn, StateOut).

%% mi_call(call(Closure, Extra...), +StateIn, -StateOut)
%% call/2..4 must also route through the meta-interpreter; otherwise a wrapped
%% task/prompt/exec runs in raw Prolog, where task_named/3 does not exist.
mi_call(call(Goal, A), StateIn, StateOut) :-
    !,
    extend_callable(Goal, [A], Callable),
    mi_call(call(Callable), StateIn, StateOut).

mi_call(call(Goal, A, B), StateIn, StateOut) :-
    !,
    extend_callable(Goal, [A, B], Callable),
    mi_call(call(Callable), StateIn, StateOut).

mi_call(call(Goal, A, B, C), StateIn, StateOut) :-
    !,
    extend_callable(Goal, [A, B, C], Callable),
    mi_call(call(Callable), StateIn, StateOut).

%% extend_callable(+Closure, +ExtraArgs, -Callable)
extend_callable(Closure, Extra, Callable) :-
    Closure =.. List,
    append(List, Extra, Extended),
    Callable =.. Extended.

%% mi_call(once(Goal), +StateIn, -StateOut)
%% once/1 must route through the meta-interpreter. As a plain builtin it would
%% run Goal in raw Prolog, so a wrapped task/prompt/exec fails with
%% task_named/3 undefined. Commits to the first solution of Goal, like once/1.
mi_call(once(Goal), StateIn, StateOut) :-
    !,
    once(mi_call(Goal, StateIn, StateOut)).

%% mi_call(ignore(Goal), +StateIn, -StateOut)
mi_call(ignore(Goal), StateIn, StateOut) :-
    !,
    (   once(mi_call(Goal, StateIn, StateOut0))
    ->  StateOut = StateOut0
    ;   StateOut = StateIn
    ).

%% mi_call(not(Goal), +StateIn, -StateOut)  (alias for \+/1)
mi_call(not(Goal), StateIn, StateIn) :-
    !,
    mi_call(\+(Goal), StateIn, StateIn).

%% Refuse reserved predicates invoked directly in non-meta frames. This is the
%% second of the three-place reserved guard (execute, mi_call, assert/load).
mi_call(Goal, StateIn, _StateOut) :-
    guard_reserved_direct(Goal, StateIn),
    !,
    throw(error(permission_error(execute, reserved_predicate, Goal),
                context(mi_call/3, 'cannot invoke a reserved REPL predicate'))).

%% guard_reserved_goal(+Goal, +StateIn)
%% Throws when Goal names a reserved predicate and the frame is not meta.
guard_reserved_goal(Goal, StateIn) :-
    (   is_dict(StateIn), get_dict(frame_kind, StateIn, meta)
    ->  true
    ;   nonvar(Goal),
        callable(Goal),
        functor(Goal, Name, _),
        repl_reserved_name(Name)
    ->  throw(error(permission_error(execute, reserved_predicate, Name),
                context(mi_call/3, 'cannot invoke a reserved REPL predicate')))
    ;   true
    ).

guard_reserved_direct(Goal, StateIn) :-
    (   is_dict(StateIn), get_dict(frame_kind, StateIn, meta)
    ->  fail
    ;   nonvar(Goal),
        callable(Goal),
        functor(Goal, Name, _),
        repl_reserved_name(Name)
    ).

%% Untransformed task/N and prompt/N (N >= 2) are source-level sugar rewritten
%% to task_named/3 / prompt_named/3 by transform_task_calls/3 at load time.
%% There is no runtime handler for the untransformed form, so it would match no
%% clause and fail silently (looking like an inexplicable "0 solutions"). Fail
%% loudly instead: reaching here means a clause was asserted/run without the
%% load-time preprocessing (assert/assert_many now apply it, so this is a guard).
mi_call(Goal, _StateIn, _StateOut) :-
    compound(Goal),
    functor(Goal, Name, Arity),
    Arity >= 2,
    ( Name == task ; Name == prompt ),
    !,
    throw(error(domain_error(unrewritten_task_form, Goal),
                context(mi_call/3, 'task/N and prompt/N are rewritten at load time (task_named/3, prompt_named/3); preprocess the clause before running it'))).

%% mi_call(Goal, +StateIn, -StateOut)
%% Catch-all for built-in and user-defined predicates
%% MUST NOT match special predicates that have dedicated handlers
mi_call(Goal, StateIn, StateOut) :-
    (   var(Goal)
    ->  throw(error(instantiation_error, mi_call/3))
    ;   true
    ),
    % Skip if this is a special predicate (has dedicated mi_call clause)
    \+ is_mi_special_predicate(Goal),
    consume_gas(StateIn, State1),
    mi_call_dispatch(Goal, State1, StateOut).

%% consume_gas(+StateIn, -StateOut)
%% Decrements gas_remaining if it is >= 0. Throws gas_exhausted if it reaches 0.
consume_gas(StateIn, StateOut) :-
    (   get_dict(gas_remaining, StateIn, Gas)
    ->  (   Gas == 0
        ->  throw(gas_exhausted)
        ;   Gas > 0
        ->  NewGas is Gas - 1,
            StateOut = StateIn.put(gas_remaining, NewGas)
        ;   % Gas is -1 (infinite)
            StateOut = StateIn
        )
    ;   % No gas limit set
        StateOut = StateIn
    ).

%% mi_call_dispatch(+Goal, +StateIn, -StateOut)
%% Dispatch user-defined vs built-in predicates
%% Uses separate clauses instead of if-then-else to preserve backtracking
mi_call_dispatch(Goal, StateIn, StateIn) :-
    get_session_id(SessionId),
    callable(Goal),
    predicate_property(SessionId:Goal, defined),
    predicate_property(Goal, built_in),
    !,
    call(SessionId:Goal).

mi_call_dispatch(Goal, StateIn, StateIn) :-
    predicate_property(Goal, built_in),
    !,  % Commit: it's built-in, no backtracking needed
    call(Goal).

mi_call_dispatch(Goal, StateIn, StateOut) :-
    get_session_id(SessionId),
    callable(Goal),
    predicate_property(SessionId:Goal, defined),
    % If it's a library/imported predicate, call it directly
    (   (predicate_property(SessionId:Goal, imported_from(_)) ; predicate_property(SessionId:Goal, foreign))
    ->  StateOut = StateIn,
        call(SessionId:Goal)
    ;   % User-defined predicate - use clause/2 for backtracking
        % Add trace entry for call
        get_depth(StateIn, Depth),
        Goal =.. [Functor|Args],
        add_trace_entry(SessionId, call, Functor, Args, Depth),
        % Find a matching clause - allows backtracking to try multiple clauses
        clause(SessionId:Goal, Body),
        NewDepth is Depth + 1,
        set_depth(StateIn, NewDepth, State1),
        % Soft cut: execute the body for every solution (so findall/backtracking
        % works), but still record a fail trace when the body has none.
        (   mi_call(Body, State1, State2)
        *-> add_trace_entry(SessionId, exit, Functor, Args, Depth),
            set_depth(State2, Depth, StateOut)
        ;   add_trace_entry(SessionId, fail, Functor, Args, Depth),
            fail
        )
    ).

mi_call_dispatch(Goal, StateIn, StateIn) :-
    % Fallback for external/library predicates
    get_session_id(SessionId),
    % Check if this predicate exists in the session module but not as user-defined
    \+ predicate_property(SessionId:Goal, defined),
    catch(call(SessionId:Goal), _, fail),
    !.

mi_call_dispatch(Goal, StateIn, StateIn) :-
    % Final fallback: call without module
    % Only for predicates not defined in session
    get_session_id(SessionId),
    \+ predicate_property(SessionId:Goal, defined),
    call(Goal).

%% ============================================================
%% Helper Predicates
%% ============================================================

%% interpolate_desc(+Template, +Params, -Result)
interpolate_desc(Template, Params, Result) :-
    (   is_dict(Params)
    ->  dict_pairs(Params, _, Pairs),
        maplist([K-V, K=V]>>true, Pairs, Bindings),
        deepclause_strings:interpolate_string(Template, Bindings, Result)
    ;   Result = Template
    ).

%% get_session_id(-SessionId)
get_session_id(SessionId) :-
    nb_current(current_session_id, SessionId), !.
get_session_id(default_session).

%% var_to_name(+Bindings, +Var, -Name)
%% Look up a variable's name from the bindings list
var_to_name(Bindings, Var, Name) :-
    (   member(N=V, Bindings), V == Var
    ->  Name = N
    ;   % format(user_error, "[DEBUG] var_to_name failed for ~w in ~w~n", [Var, Bindings]),
        Name = 'Var'  % Fallback if not found
    ).

%% ============================================================
%% Compile-Time String Interpolation Expansion
%% ============================================================

%% expand_interpolations(+Term, +Bindings, -ExpandedTerm)
%% Walk a term and expand strings containing {VarName} patterns
%% Bindings is a list of 'VarName'=Variable pairs from read_term
%% Interpolation happens in goal position only: `task/N`/`prompt/N`/`output/1`/… and
%% the arguments of a user goal, never inside a list, a dict, or another compound
%% passed as data. See the note on expand_term_interp/3.
expand_interpolations((Head :- Body), Bindings, (Head :- ExpandedBody)) :-
    !,
    expand_body_interp(Body, Bindings, ExpandedBody).
expand_interpolations(Term, Bindings, ExpandedTerm) :-
    expand_body_interp(Term, Bindings, ExpandedTerm).

%% expand_term_interp(+Term, +Bindings, -Expanded)
%% Main term expansion predicate

% Variables pass through unchanged
expand_term_interp(Var, _, Var) :-
    var(Var), !.

% Strings are LITERAL here. Interpolation is decided by expand_body_interp/3, which
% only rewrites a string that is a *direct argument of a goal*. Rewriting every
% string, nested ones included, corrupted data: the shell command
%   exec(bash(argv: [..., "-w", "%{http_code}", Url]), R)
% had its `%{http_code}` element replaced by a format goal inside the argv list, so
% the list arrived truncated at `-w`. A JSON document, a regex, an `awk` program and
% a `jq` filter all contain braces and must survive verbatim.
expand_term_interp(String, _, String) :-
    string(String),
    !.

% Atoms - check if they need interpolation (for atom strings)
expand_term_interp(Atom, Bindings, Expanded) :-
    atom(Atom),
    \+ Atom = [],  % Not empty list
    atom_string(Atom, String),
    string_needs_interpolation(String), !,
    build_format_goal(String, Bindings, ExpandedGoal),
    % Wrap in atom conversion if original was atom
    Expanded = ExpandedGoal.

% Regular atoms pass through
expand_term_interp(Atom, _, Atom) :-
    atom(Atom), !.

% Numbers pass through
expand_term_interp(Num, _, Num) :-
    number(Num), !.

% Empty list
expand_term_interp([], _, []) :- !.

% Lists - expand each element
expand_term_interp([H|T], Bindings, [EH|ET]) :- !,
    expand_term_interp(H, Bindings, EH),
    expand_term_interp(T, Bindings, ET).

% Clause with body - special handling for string interpolation in goals
expand_term_interp((Head :- Body), Bindings, (Head :- ExpandedBody)) :- !,
    expand_body_interp(Body, Bindings, ExpandedBody).

% Other compound terms - expand arguments
expand_term_interp(Term, Bindings, ExpandedTerm) :-
    compound(Term), !,
    Term =.. [Functor|Args],
    maplist({Bindings}/[A, EA]>>expand_term_interp(A, Bindings, EA), Args, ExpandedArgs),
    ExpandedTerm =.. [Functor|ExpandedArgs].

%% expand_body_interp(+Body, +Bindings, -ExpandedBody)
%% Expand goals in a clause body, handling control structures

% Variable goal
expand_body_interp(Var, _, Var) :-
    var(Var), !.

% A list or a dict is DATA, not a sequence of goals. Descending into one treats its
% elements as goal arguments and hoists any `{...}` string it contains, which
% rewrote data into a goal term:
%   argv: ["curl","-w","%{http_code}","u"]   became
%   argv: [curl,-w|((param(http_code,_),format(...)),[Temp,u])]
% so `exec(bash(argv: [...]))` reached the shell with a truncated argument list.
expand_body_interp(Data, _, Data) :-
    (   Data == []
    ;   Data = [_|_]
    ;   is_dict(Data)
    ),
    !.

% Conjunction
expand_body_interp((A, B), Bindings, ExpandedConj) :- !,
    expand_body_interp(A, Bindings, EA),
    expand_body_interp(B, Bindings, EB),
    flatten_conjunction(EA, EB, ExpandedConj).

% Disjunction  
expand_body_interp((A ; B), Bindings, (EA ; EB)) :- !,
    expand_body_interp(A, Bindings, EA),
    expand_body_interp(B, Bindings, EB).

% If-then-else
expand_body_interp((A -> B ; C), Bindings, (EA -> EB ; EC)) :- !,
    expand_body_interp(A, Bindings, EA),
    expand_body_interp(B, Bindings, EB),
    expand_body_interp(C, Bindings, EC).

% If-then
expand_body_interp((A -> B), Bindings, (EA -> EB)) :- !,
    expand_body_interp(A, Bindings, EA),
    expand_body_interp(B, Bindings, EB).

% Soft cut
expand_body_interp((A *-> B), Bindings, (EA *-> EB)) :- !,
    expand_body_interp(A, Bindings, EA),
    expand_body_interp(B, Bindings, EB).

% Negation
expand_body_interp(\+(A), Bindings, \+(EA)) :- !,
    expand_body_interp(A, Bindings, EA).

% Goals that take string arguments - expand the string arg specially
expand_body_interp(Goal, Bindings, ExpandedGoal) :-
    goal_with_string_arg(Goal, Functor, StringArg, RestArgs), !,
    (   string_needs_interpolation(StringArg)
    ->  build_format_call(StringArg, Bindings, TempVar, FormatGoal),
        rebuild_goal(Functor, TempVar, RestArgs, NewGoal),
        ExpandedGoal = (FormatGoal, NewGoal)
    ;   expand_rest_args(RestArgs, Bindings, ExpandedRestArgs),
        rebuild_goal(Functor, StringArg, ExpandedRestArgs, ExpandedGoal)
    ).

% Goals whose interpolated string is NOT the first argument (verify/3, judge/2,
% choose/4, rate/4, probability/3, holds/2,3, and any user predicate): hoist the
% interpolation into a preceding goal.
expand_body_interp(Goal, Bindings, ExpandedGoal) :-
    compound(Goal),
    Goal =.. [Functor|Args],
    interp_arg_predicate(Functor),
    expand_args_hoist(Args, Bindings, PreGoals, ExpandedArgs, true),
    !,
    Bare =.. [Functor|ExpandedArgs],
    prepend_goals(PreGoals, Bare, ExpandedGoal).

% Other goals - expand arguments
expand_body_interp(Goal, Bindings, ExpandedGoal) :-
    compound(Goal), !,
    Goal =.. [Functor|Args],
    maplist({Bindings}/[A, EA]>>expand_body_interp(A, Bindings, EA), Args, ExpandedArgs),
    ExpandedGoal =.. [Functor|ExpandedArgs].

% Atoms/other
expand_body_interp(Goal, _, Goal).

%% Predicates whose string arguments interpolate `{Var}`. Interpolation lives in
%% the *description* of an LLM step and in the question of a judgment; anywhere else
%% a string is data and must survive verbatim. Hoisting for every goal made a string
%% of data into a goal argument: `X = "%{code}"` became an interpolation of the
%% variable `code`, and a shell argv containing `%{http_code}` was rewritten.
%%
%% (task/N, prompt/N, answer/1, system/1, user/1, output/1, log/1 and yield/1 are
%% handled by goal_with_string_arg/4 above, for their first argument.)
interp_arg_predicate(judge).
interp_arg_predicate(choose).
interp_arg_predicate(rate).
interp_arg_predicate(verify).
interp_arg_predicate(probability).
interp_arg_predicate(holds).

%% goal_with_string_arg(+Goal, -Functor, -StringArg, -RestArgs)
%% Match goals that take a string as their first argument
goal_with_string_arg(answer(S), answer, S, []) :- string(S).
goal_with_string_arg(answer(S), answer, S, []) :- atom(S), \+ S = [].
goal_with_string_arg(system(S), system, S, []) :- string(S).
goal_with_string_arg(system(S), system, S, []) :- atom(S), \+ S = [].
goal_with_string_arg(user(S), user, S, []) :- string(S).
goal_with_string_arg(user(S), user, S, []) :- atom(S), \+ S = [].
goal_with_string_arg(output(S), output, S, []) :- string(S).
goal_with_string_arg(output(S), output, S, []) :- atom(S), \+ S = [].
goal_with_string_arg(log(S), log, S, []) :- string(S).
goal_with_string_arg(log(S), log, S, []) :- atom(S), \+ S = [].
goal_with_string_arg(yield(S), yield, S, []) :- string(S).
goal_with_string_arg(yield(S), yield, S, []) :- atom(S), \+ S = [].
goal_with_string_arg(task(S), task, S, []) :- string(S).
goal_with_string_arg(task(S), task, S, []) :- atom(S), \+ S = [].
goal_with_string_arg(task(S, V1), task, S, [V1]) :- string(S).
goal_with_string_arg(task(S, V1), task, S, [V1]) :- atom(S), \+ S = [].
goal_with_string_arg(task(S, V1, V2), task, S, [V1, V2]) :- string(S).
goal_with_string_arg(task(S, V1, V2), task, S, [V1, V2]) :- atom(S), \+ S = [].
goal_with_string_arg(task(S, V1, V2, V3), task, S, [V1, V2, V3]) :- string(S).
goal_with_string_arg(task(S, V1, V2, V3), task, S, [V1, V2, V3]) :- atom(S), \+ S = [].
%% task_named/3 - transformed form with embedded variable names
goal_with_string_arg(task_named(S, Vars, Names), task_named, S, [Vars, Names]) :- string(S).
goal_with_string_arg(task_named(S, Vars, Names), task_named, S, [Vars, Names]) :- atom(S), \+ S = [].
goal_with_string_arg(prompt(S), prompt, S, []) :- string(S).
goal_with_string_arg(prompt(S), prompt, S, []) :- atom(S), \+ S = [].
goal_with_string_arg(prompt(S, V1), prompt, S, [V1]) :- string(S).
goal_with_string_arg(prompt(S, V1), prompt, S, [V1]) :- atom(S), \+ S = [].
goal_with_string_arg(prompt(S, V1, V2), prompt, S, [V1, V2]) :- string(S).
goal_with_string_arg(prompt(S, V1, V2), prompt, S, [V1, V2]) :- atom(S), \+ S = [].
goal_with_string_arg(prompt(S, V1, V2, V3), prompt, S, [V1, V2, V3]) :- string(S).
goal_with_string_arg(prompt(S, V1, V2, V3), prompt, S, [V1, V2, V3]) :- atom(S), \+ S = [].
%% prompt_named/3 - transformed form with embedded variable names
goal_with_string_arg(prompt_named(S, Vars, Names), prompt_named, S, [Vars, Names]) :- string(S).
goal_with_string_arg(prompt_named(S, Vars, Names), prompt_named, S, [Vars, Names]) :- atom(S), \+ S = [].
goal_with_string_arg(sample_token(S, Token), sample_token, S, [Token]) :- string(S).
goal_with_string_arg(sample_token(S, Token), sample_token, S, [Token]) :- atom(S), \+ S = [].
goal_with_string_arg(sample_token(S, Allowed, Token), sample_token, S, [Allowed, Token]) :- string(S).
goal_with_string_arg(sample_token(S, Allowed, Token), sample_token, S, [Allowed, Token]) :- atom(S), \+ S = [].

%% rebuild_goal(+Functor, +StringArg, +RestArgs, -Goal)
%% Rebuild a goal with the string argument and rest args
rebuild_goal(Functor, StringArg, [], Goal) :- !,
    Goal =.. [Functor, StringArg].
rebuild_goal(Functor, StringArg, RestArgs, Goal) :-
    Goal =.. [Functor, StringArg | RestArgs].

%% expand_rest_args(+Args, +Bindings, -ExpandedArgs)
expand_rest_args([], _, []).
expand_rest_args([A|As], Bindings, [EA|EAs]) :-
    expand_body_interp(A, Bindings, EA),
    expand_rest_args(As, Bindings, EAs).

%% expand_args_hoist(+Args, +Bindings, -PreGoals, -Args2, -Hoisted)
%% Expand each argument; any interpolated string becomes a goal in PreGoals and
%% its argument becomes the temp variable that goal binds.
expand_args_hoist([], _, [], [], false).
expand_args_hoist([Arg|Args], Bindings, PreGoals, [Expanded|EArgs], Hoisted) :-
    expand_arg_hoist(Arg, Bindings, Pre, Expanded, ThisHoisted),
    expand_args_hoist(Args, Bindings, Rest, EArgs, RestHoisted),
    append(Pre, Rest, PreGoals),
    (   ThisHoisted == true
    ->  Hoisted = true
    ;   Hoisted = RestHoisted
    ).

expand_arg_hoist(Arg, Bindings, [FormatGoal], TempVar, true) :-
    (   string(Arg)
    ;   atom(Arg)
    ),
    string_needs_interpolation(Arg),
    !,
    build_format_call(Arg, Bindings, TempVar, FormatGoal).
expand_arg_hoist(Arg, _, [], Arg, false).

%% prepend_goals(+PreGoals, +Goal, -Expanded)
prepend_goals([], Goal, Goal) :- !.
prepend_goals(Goals, Goal, Expanded) :-
    list_to_conjunction(Goals, Conj),
    Expanded = (Conj, Goal).

%% flatten_conjunction(+A, +B, -Conj)
%% Flatten nested conjunctions properly
flatten_conjunction((A1, A2), B, Result) :- !,
    flatten_conjunction(A2, B, Rest),
    Result = (A1, Rest).
flatten_conjunction(A, B, (A, B)).

%% string_needs_interpolation(+String)
%% Check if a string contains identifier-like {VarName} patterns
string_needs_interpolation(String) :-
    (   string(String) -> S = String
    ;   atom(String) -> atom_string(String, S)
    ;   fail
    ),
    string_codes(S, Codes),
    extract_vars_from_codes(Codes, VarNames, _),
    VarNames \= [].

%% build_format_call(+Template, +Bindings, -TempVar, -FormatGoal)
%% Build a format/3 call that produces the interpolated string
%% For variables in scope: use the variable directly
%% For params (not in scope): generate param(name, Var) lookup
build_format_call(Template, Bindings, TempVar, FullGoal) :-
    (   string(Template) -> T = Template
    ;   atom_string(Template, T)
    ),
    extract_interpolation_vars(T, VarNames, FormatString),
    lookup_vars_with_params(VarNames, Bindings, VarList, ParamGoals),
    FormatGoal = format(string(TempVar), FormatString, VarList),
    (   ParamGoals == []
    ->  FullGoal = FormatGoal
    ;   list_to_conjunction(ParamGoals, ParamConj),
        FullGoal = (ParamConj, FormatGoal)
    ).

%% list_to_conjunction(+List, -Conjunction)
list_to_conjunction([G], G) :- !.
list_to_conjunction([G|Gs], (G, Rest)) :-
    list_to_conjunction(Gs, Rest).

%% build_format_goal(+String, +Bindings, -Goal)
%% For direct string expansion (not in a goal context)
build_format_goal(String, Bindings, Goal) :-
    build_format_call(String, Bindings, TempVar, FormatGoal),
    Goal = (FormatGoal, TempVar).

%% extract_interpolation_vars(+Template, -VarNames, -FormatString)
%% Parse template to extract variable names and build format string
extract_interpolation_vars(Template, VarNames, FormatString) :-
    string_codes(Template, Codes),
    extract_vars_from_codes(Codes, VarNames, FormatCodes),
    string_codes(FormatString, FormatCodes).

%% extract_vars_from_codes(+Codes, -VarNames, -FormatCodes)
extract_vars_from_codes([], [], []) :- !.

% Found opening brace
extract_vars_from_codes([0'{|Rest], [VarName|VarNames], [0'~, 0'w|FormatRest]) :-
    extract_var_until_close(Rest, VarNameCodes, AfterClose),
    valid_placeholder_codes(VarNameCodes),
    !,
    atom_codes(VarName, VarNameCodes),
    extract_vars_from_codes(AfterClose, VarNames, FormatRest).

% Invalid placeholder syntax - keep the opening brace literal
extract_vars_from_codes([0'{|Rest], VarNames, [0'{|FormatRest]) :- !,
    extract_vars_from_codes(Rest, VarNames, FormatRest).

% Escape tilde for format/3
extract_vars_from_codes([0'~|Rest], VarNames, [0'~, 0'~|FormatRest]) :- !,
    extract_vars_from_codes(Rest, VarNames, FormatRest).

% Regular character
extract_vars_from_codes([C|Rest], VarNames, [C|FormatRest]) :-
    extract_vars_from_codes(Rest, VarNames, FormatRest).

%% extract_var_until_close(+Codes, -VarNameCodes, -Rest)
extract_var_until_close([0'}|Rest], [], Rest) :- !.
extract_var_until_close([C|Rest], [C|VarRest], Final) :-
    extract_var_until_close(Rest, VarRest, Final).
extract_var_until_close([], [], []).

%% valid_placeholder_codes(+Codes)
%% Placeholder names must look like identifiers: [A-Za-z_][A-Za-z0-9_]*
valid_placeholder_codes([First|Rest]) :-
    valid_placeholder_start(First),
    maplist(valid_placeholder_char, Rest).

valid_placeholder_start(Code) :-
    code_type(Code, alpha)
    ; Code =:= 0'_.

valid_placeholder_char(Code) :-
    code_type(Code, alnum)
    ; Code =:= 0'_.

%% lookup_vars_with_params(+VarNames, +Bindings, -VarList, -ParamGoals)
%% Look up each variable name:
%% - If in Bindings (local Prolog variable) → use directly
%% - If not in Bindings → generate param(name, Var) lookup
lookup_vars_with_params([], _, [], []).
lookup_vars_with_params([Name|Names], Bindings, [Var|Vars], ParamGoals) :-
    (   member(Name=Var, Bindings)
    ->  % Found as local variable
        ParamGoals = RestGoals
    ;   % Not a local variable - treat as param lookup
        % Convert to lowercase atom for param key
        downcase_atom(Name, LowerName),
        ParamGoals = [param(LowerName, Var)|RestGoals]
    ),
    lookup_vars_with_params(Names, Bindings, Vars, RestGoals).

