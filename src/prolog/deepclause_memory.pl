/**
 * deepclause_memory.pl - DEPRECATED Memory management for DeepClause SDK
 * 
 * NOTE: As of v0.2.0, memory is managed via state threading in the 
 * meta-interpreter for proper backtracking support. This module is
 * kept for backwards compatibility only.
 * 
 * Memory is now automatically backtrackable - when Prolog backtracks,
 * the memory state is automatically restored. No need for manual
 * push_context/pop_context calls.
 */

:- module(deepclause_memory, [
    create_memory/1,
    push_memory/2,
    pop_memory/1,
    get_memory/2,
    clear_memory/1,
    push_context/1,
    pop_context/1
]).

%% All operations are now no-ops or return empty results
%% Memory is managed internally by the meta-interpreter

%% create_memory(+Id) - No-op, kept for API compatibility
create_memory(_Id) :- true.

%% push_memory(+Id, +Message) - No-op, memory is managed by MI state
push_memory(_Id, _Message) :- true.

%% pop_memory(+Id) - No-op
pop_memory(_Id) :- true.

%% get_memory(+Id, -Messages) - Returns empty, memory comes from MI state
get_memory(_Id, []).

%% clear_memory(+Id) - No-op
clear_memory(_Id) :- true.

%% push_context(+Id) - No-op, backtracking is automatic
push_context(_Id) :- true.

%% pop_context(+Id) - No-op, backtracking is automatic
pop_context(_Id) :- true.
