%% Host capability tools for the plain runtime.
%%
%% The only initial capability is `bash`; `ask_user` is also supported because
%% the MI exposes it through exec/2.
:- module(dc_tools, [exec_payload/3, run_bash/3, args_to_dict/3]).

:- use_module(library(process)).
:- use_module(library(lists)).
:- use_module(dc_config).
:- use_module(dc_json).
:- use_module(dc_events).

%% exec_payload(+Payload, -Result, -Success)
exec_payload(Payload, Result, Success) :-
    (   is_dict(Payload), get_dict(toolName, Payload, Tool0)
    ->  dc_json:as_atom(Tool0, Tool)
    ;   Tool = unknown
    ),
    (   is_dict(Payload), get_dict(args, Payload, Args)
    ->  true
    ;   Args = []
    ),
    args_to_dict(Tool, Args, Options),
    exec_tool(Tool, Options, Result, Success).

%% args_to_dict(+Tool, +Args, -Dict)
%% MI passes `exec(bash(command: "...", timeoutMs: N), R)` as a list of `Key:Value`
%% compounds. Positional args are accepted for bash/ask_user convenience.
args_to_dict(_, Args, Dict) :-
    is_dict(Args),
    !,
    Dict = Args.
args_to_dict(_, Args, Dict) :-
    is_list(Args),
    Args \== [],
    forall(member(A, Args), (is_dict(A) ; named_arg(A))),
    !,
    named_args(Args, Dict).
args_to_dict(bash, [Command|_], Dict) :-
    !,
    dc_json:as_text(Command, Cmd),
    Dict = _{command: Cmd}.
args_to_dict(ask_user, [Prompt|_], Dict) :-
    !,
    dc_json:as_text(Prompt, Text),
    Dict = _{prompt: Text}.
args_to_dict(_, _, _{}).

named_arg(Term) :- Term = (_:_).
named_arg(Term) :- Term = (_=_).

named_args(Args, Dict) :-
    findall(Key-Value,
        (   member(Arg, Args),
            (   Arg = (K:V)
            ;   Arg = (K=V)
            ),
            dc_json:key_atom(K, Key),
            Value = V
        ),
        Pairs),
    dict_create(Dict, _, Pairs).

exec_tool(bash, Options, Result, true) :-
    !,
    (   get_dict(command, Options, Command0)
    ->  dc_json:as_text(Command0, Command)
    ;   get_dict(argv, Options, Argv), is_list(Argv)
    ->  argv_command(Argv, Command)
    ;   Command = "true"
    ),
    (   get_dict(timeoutMs, Options, Timeout0), integer(Timeout0), Timeout0 > 0
    ->  TimeoutMs = Timeout0
    ;   TimeoutMs = 120000
    ),
    run_bash(Command, TimeoutMs, Result).
exec_tool(ask_user, Options, Result, true) :-
    !,
    (   get_dict(prompt, Options, Prompt0)
    ->  dc_json:as_text(Prompt0, Prompt)
    ;   Prompt = "Continue?"
    ),
    format("~w~n> ", [Prompt]),
    flush_output,
    (   read_line_to_string(user_input, Line)
    ->  ( Line == end_of_file -> Answer = "" ; Answer = Line )
    ;   Answer = ""
    ),
    Result = Answer.
exec_tool(Tool, _, Result, false) :-
    format(string(Message), "Tool not found: ~w", [Tool]),
    Result = _{error: Message}.

argv_command(Argv, Command) :-
    maplist(quote_shell_arg, Argv, Quoted),
    atomic_list_concat(Quoted, " ", Command).

quote_shell_arg(Value, Quoted) :-
    dc_json:as_text(Value, Text),
    split_string(Text, "'", "", Parts),
    atomic_list_concat(Parts, "'\\''", Joined),
    atomic_list_concat(["'", Joined, "'"], Quoted).

%% run_bash(+Command, +TimeoutMs, -Result)
run_bash(Command, TimeoutMs, Result) :-
    dc_config:workspace(Workspace),
    shell_executable(Shell),
    shell_env(Workspace, Env),
    (   catch(process_create(Shell, ['-c', Command],
            [ cwd(Workspace),
              stdout(pipe(Out)),
              stderr(pipe(Err)),
              process(Pid),
              env(Env),
              detached(true)
            ]), Error, (format(string(_), "failed to start shell: ~w", [Error]), fail))
    ->  get_time(Start),
        message_queue_create(Queue),
        thread_create(read_all_queue(Out, Queue, out), ReaderOut, []),
        thread_create(read_all_queue(Err, Queue, err), ReaderErr, []),
        wait_with_timeout(Pid, TimeoutMs, Start, Status),
        thread_join(ReaderOut, _),
        thread_join(ReaderErr, _),
        thread_get_message(Queue, out(OutText)),
        thread_get_message(Queue, err(ErrText)),
        message_queue_destroy(Queue),
        (   Status == timeout
        ->  ExitCode = 124,
            TimeoutMessage = "command timed out",
            ( ErrText == "" -> Stderr = TimeoutMessage
            ;   string_concat(ErrText, "\n", Tmp),
                string_concat(Tmp, TimeoutMessage, Stderr)
            )
        ;   status_exit(Status, ExitCode0),
            ExitCode = ExitCode0,
            Stderr = ErrText
        ),
        ( ExitCode =:= 0 -> Success = true ; Success = false ),
        Result = _{success: Success, exitCode: ExitCode,
                   stdout: OutText, stderr: Stderr}
    ;   Result = _{success: false, exitCode: 127, stdout: "",
                 stderr: "failed to start shell"}
    ).

read_all_queue(Stream, Queue, Tag) :-
    catch(read_string(Stream, _, Text0), _, Text0 = ""),
    close(Stream),
    (   string(Text0) -> Text = Text0 ; Text = "" ),
    Message =.. [Tag, Text],
    thread_send_message(Queue, Message).

wait_with_timeout(Pid, TimeoutMs, Start, Status) :-
    process_wait(Pid, S, [timeout(0)]),
    (   S == timeout
    ->  get_time(Now),
        ElapsedMs is (Now - Start) * 1000,
        (   TimeoutMs > 0, ElapsedMs >= TimeoutMs
        ->  catch(process_group_kill(Pid, 15), _, true),
            catch(process_wait(Pid, _), _, true),
            Status = timeout
        ;   sleep(0.05),
            wait_with_timeout(Pid, TimeoutMs, Start, Status)
        )
    ;   Status = S
    ).

status_exit(exit(Code), Code) :- !.
status_exit(killed(_), 128) :- !.
status_exit(_, 1).

shell_executable(Shell) :-
    (   exists_file('/bin/bash')
    ->  Shell = '/bin/bash'
    ;   Shell = '/bin/sh'
    ).

shell_env(Workspace, Env) :-
    (   getenv('PATH', Path0), Path0 \== ""
    ->  Path = Path0
    ;   Path = "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin"
    ),
    (   getenv('HOME', Home0), Home0 \== ""
    ->  Home = Home0
    ;   Home = Workspace
    ),
    (   getenv('TMPDIR', Tmp0), Tmp0 \== ""
    ->  Tmp = Tmp0
    ;   Tmp = "/tmp"
    ),
    Env = [ 'PATH'=Path, 'HOME'=Home, 'TMPDIR'=Tmp, 'WORKSPACE'=Workspace, 'LANG'="C.UTF-8" ].
