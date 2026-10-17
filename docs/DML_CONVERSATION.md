# Conversational agents

A **conversation** is a harness that talks with the person over many turns and
acts between them. Two primitives, `input/1` and `chat/1`, are the whole loop.

> **Status.** `input/1`, `chat/1` and `chat/2` are implemented.
> **Events into a conversation (§5) are proposed, not built.** Everything else
> here describes shipped behaviour.

```prolog
agent_main :-
    system("You are a support assistant. Be brief."),
    loop.

loop :-
    input(A),        % the person's next message; fails when there is none
    chat(A),         % one turn: tools, then it replies and stops
    loop.
```

## 1. Why not `task/N`

`task/N` is a **subtask**: it has a goal, and it must hand back a result. So it
carries `set_result` and `finish`, it nags the model when it has not finished, and
it summarises its own conversation for the next call.

A conversation is the opposite:

| | `task/N` | `chat/1` |
|---|---|---|
| driven by | a description with a goal | the person's message |
| ends when | the model calls `finish` | the model stops calling tools |
| results | `set_result` for each output var | none — the reply *is* the answer |
| nudging | yes (a nudge when the model stops early) | never |
| memory | a summary for the next task | the conversation itself |

So `chat/1` is a **traditional agent loop**: the model gets the tools, does what
it needs, replies, and stops. The next message starts the next turn.

## 2. The primitives

### `input(-Message)`

Wait for the person's next message.

- **No prompt.** The embedding UI is a chat interface; it is already asking. The
  host gets `wait_input` with an empty prompt and reads a line (or the next
  message from its own chat widget).
- **Fails when there is none.** In a non-interactive run (`-g`, `--json`, CI,
  piped stdin) `input/1` fails rather than returning `""`. That is what ends
  `loop :- input(A), chat(A), loop.` — a conversation never spins on empty turns.
  Guard it (`-> ... ; true`) when the rest of the harness should still run.

`input/2` is unchanged: it has a prompt and is for a question inside a task.

### `chat(+Message)` / `chat(+Message, -Reply)`

One conversational turn.

- `Message` is appended as the turn's user message (with `{Param}` interpolation,
  like `task/N`).
- The loop runs with **every `tool/N` the harness declares**, plus the built-in
  ones. Tool results are fed back; the model may call several.
- The turn ends when the model stops calling tools. The text it produced is
  streamed to the UI as it arrives (`stream` events) and kept in memory.
- `chat/2` binds the last assistant message, for a harness that wants to post
  process it.
- **No `finish`, no `set_result`, no nudging.** A conversational agent that must
  produce a *value* is a `task/N` (with a [form](DML_FORMS.md) if the value comes
  from the person), not a chat.

Memory is the conversation: the next `chat/1` sees what was said, because the
loop returns the full transcript and it is stored as the session memory. `system/1`
before the loop sets the persona and survives every turn.

## 3. It must be the foreground goal

A conversation blocks on a person, so it is the thing the user runs — `/goal
agent_main`, `-g agent_main`, or the embedding host's run. Not `execute` inside a
turn, and not a subagent: those would leave the prompt waiting on something
nobody can answer.

**Interaction with forms.** A turn may open a form (`render_form`) — the person
fills it in, the turn continues. That is the natural way to collect structured
input *inside* a conversation; see [DML_FORMS.md](DML_FORMS.md).

## 4. What the host must provide

- an input channel that answers `wait_input` (`onUserInput` / the CLI's editor /
  a chat widget), and can answer "no more input";
- streaming, so the reply appears as it is generated (otherwise the whole reply
  arrives at the end of the turn — correct, but it feels dead);
- a transcript, so the conversation is visible above the input line.

## 5. Events: acting on a worker's message mid-conversation *(proposed)*

### The problem

The point of a conversational agent is that it is *idle but present*. Meanwhile a
worker does something slow — checks mail every five minutes, waits for a build, a
long job finishes. The conversation should be able to **react**: act (tools) and
say something in the chat, without the person having typed anything.

Today that is half-built and awkward. A session **is** a process: it has a pid
(`self(Me)`), a mailbox, and `receive/2` works in the foreground — a spawned
worker can `send(Me, mail(M))` and the foreground receives it. But `input/1` and
`receive/1` both block, and there is no way to wait on both. So a harness must
poll, and a poll cannot be the conversation guard:

```prolog
loop :-                             % what you would have to write today
    (   receive(Event, 200)
    ->  handle(Event), loop
    ;   input(A), chat(A), loop
    ).
```

That is wrong in two ways: a 200 ms tick eats the turn, and a message that
arrives while the person is typing waits behind the input.

### The shape of a fix

Both sources are *inputs to the same loop*. The human's next message and a
worker's message differ only in **who sent it**. So:

**One inbox.** The session's mailbox is the conversation's inbox. The UI delivers
the person's message into it (`user(Text)`); a worker `send`s (`message(Term)`).
One predicate reads the next item, whichever kind it is:

```prolog
next(-Item).        % next(Item): user(Text) | message(Term) | system(Text)
input(-Text).       % sugar: next(user(Text))      — implemented today
```

Then the conversation writes an explicit dispatch loop:

```prolog
loop :-
    next(Item),
    handle(Item),
    loop.

handle(user(Text)) :- chat(Text).
handle(message(mail(M))) :-
    format(string(Note), "A new email arrived from ~w.", [M]),
    system(Note),
    chat("Handle the new email.").     % one normal turn; tools; replies in the chat
```

`chat/1` here is the same one the person triggers — the event turn is a normal
turn, visible in the transcript, with the same tools and the same validation.
That is the property worth keeping: **an event does not get a second, privileged
path into the agent.**

### Why not `on(Pattern) :- Body`

The obvious sugar is declarative handlers:

```prolog
on(mail(M)) :- format(...), chat("Handle the new email.").
```

It reads well, and it is what an Erlang-flavoured design would suggest. It is
still the wrong *first* step:

- **It hides control flow.** Which handler runs, in what order, and whether it can
  run *while* the harness is blocked in `input/1` are questions with no good
  answer. The explicit `next/1` loop answers them by construction.
- **It needs re-entrancy.** A handler firing during `input/1` means a nested frame
  on top of a blocked parent, with the chat turn inside it. The runtime can do it
  (`execute` already nests frames), but it is the most delicate part of the
  engine, and the failure mode is a deadlock rather than an error.
- **It buys nothing `next/1` cannot do.** `next/1` + a `handle/1` clause is three
  extra lines and covers the same ground, including "two workers, different
  events".

So: `next/1` first. If harnesses converge on the same `handle/1` shape, `on/1`
can be added later as sugar over it, with the explicit loop as the contract.

### A worker that wakes a conversation

```prolog
agent_main :- system("You are a mail assistant."), self(Me), loop(Me).

loop(Me) :-
    spawn_opt(mail_watcher(Me), _, [name(mail)]),
    converse(Me).

converse(Me) :-
    next(Item), handle(Item), converse(Me).

handle(user(Text)) :- chat(Text).
handle(message(mail(M))) :- system("New email."), chat("Summarise the new email: {M}").

mail_watcher(Me) :-
    sleep(300000),                    % five minutes
    check_mail(M),
    ( M == none -> true ; send(Me, mail(M)) ),
    mail_watcher(Me).
```

### Semantics to pin down

- **Ordering.** Events are queued, never dropped. An item that arrives mid-turn
  waits for the turn to finish; the loop then reads it in order.
- **One turn at a time.** No concurrent agent turns against one memory.
- **Not a silent action.** An event-triggered turn is a turn: it appears in the
  transcript, and the person sees what the agent did and why. A turn that would
  say nothing is suspicious — prefer a one-line note.
- **Backpressure.** The inbox is bounded; the oldest events are summarised
  (`system`) rather than dropped, so the agent knows it missed some.
- **Cancellation.** `/stop` (or the host's abort) ends a turn; queued events stay
  queued for the next turn.
- **Delivery is at-least-once.** A worker that retries on timeout may deliver the
  same event twice; handlers should be idempotent, or the event should carry a
  key.

### What is missing to build it

1. `seed` the session's mailbox from the UI: the input channel posts
   `user(Text)` into the same mailbox instead of resolving `wait_input`.
2. `next/1` in the MI (a `receive` that also unblocks on UI input).
3. A way for a harness to *want* events: today a message to the foreground pid is
   received only if the harness calls `receive`. With `next/1` it is the loop.
4. An inbox cap and a "you missed N events" summary.

None of that changes the conversation contract in §2; `input/1` and `chat/1` stay
exactly as they are, and `next/1` is additive.
