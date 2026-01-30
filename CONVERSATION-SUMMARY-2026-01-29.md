# Conversation Summary - January 29, 2026

## Session Focus: Deep Research Agent Bug Fixes and Memory Threading

This session addressed several issues with the DeepClause SDK, particularly around the deep research agent example and memory management across task() calls.

---

## Issues Fixed

### 1. Memory Threading Bug (Main Issue)

**Problem**: The deep research agent was repeating searches in Phase 2 that had already been done in Phase 1. The debug_memory.json only showed a few messages, missing all the intermediate tool calls and results.

**Root Cause**: 
- The agent loop was returning ALL messages including system messages
- System messages contain task-specific instructions that change for each task
- When stored in Prolog memory, the old task instructions confused the next task
- Additionally, Prolog was only storing `task description + final response`, not the full conversation

**Solution** (multiple files):

1. **agent.ts** - Filter out system messages from returned messages:
```typescript
// Return only non-system messages for memory persistence
// System messages are task-specific and rebuilt for each task
const persistentMessages = messages
  .filter(m => m.role !== 'system')
  .map(m => ({
    role: m.role as 'user' | 'assistant',
    content: typeof m.content === 'string' ? m.content : JSON.stringify(m.content),
  }));
```

2. **runner.ts** - Pass full messages to Prolog:
```typescript
private postAgentResult(result: {
  success: boolean;
  outputs: string[];
  variables: Record<string, unknown>;
  messages: Array<{ role: 'user' | 'assistant'; content: string }>;
}): void {
  // Convert messages to Prolog list format
  const messagesStr = result.messages
    .map(m => `message{role: ${m.role}, content: ${this.toPrologTerm(m.content)}}`)
    .join(', ');

  this.query(
    `deepclause_mi:post_agent_result('${this.sessionId}', ${result.success}, vars{${varsStr}}, [${messagesStr}])`
  );
}
```

3. **deepclause_mi.pl** - Accept and use full messages:
```prolog
%% Updated export
post_agent_result/4,  % was /3

%% Updated predicate
post_agent_result(SessionId, Success, Variables, Messages) :-
    assertz(session_agent_result(SessionId, result{success: Success, variables: Variables, messages: Messages})),
    session_engine(SessionId, Engine),
    engine_post(Engine, agent_done).

%% Updated task handling to use full messages
mi_call(task(Desc), StateIn, StateOut) :-
    ...
    % Use full messages from agent loop result
    (   get_dict(messages, Result, Messages), Messages \= []
    ->  set_memory(StateIn, Messages, StateOut)
    ;   % Fallback to old behavior
        ...
    ).
```

### 2. AgentLoopResult Interface Update

Updated the return type to reflect that system messages are excluded:

```typescript
export interface AgentLoopResult {
  success: boolean;
  outputs: string[];
  variables: Record<string, unknown>;
  /** Conversation messages from the agent loop (excludes system messages which are task-specific) */
  messages: Array<{ role: 'user' | 'assistant'; content: string }>;
}
```

---

## Files Modified

| File | Changes |
|------|---------|
| `deepclause-sdk/src/agent.ts` | Filter system messages from returned messages, update interface |
| `deepclause-sdk/src/runner.ts` | Pass messages to Prolog via `postAgentResult`, update `currentMemory` |
| `deepclause-sdk/src/prolog-src/deepclause_mi.pl` | Accept messages in `post_agent_result/4`, use them in `mi_call(task(...))` |

---

## Testing Results

After fixes, the deep research agent:
- ✅ Completes all 3 phases without repeating searches
- ✅ Phase 1: Initial exploration and subtopic identification
- ✅ Phase 2: Writing and saving the research report  
- ✅ Phase 3: Creating executive summary
- ✅ Debug memory now contains 28 messages (was only 11 before)
- ✅ Includes all intermediate tool calls and results

---

## Key Insight

The critical insight was understanding that **system messages are task-specific**. Each `task()` call generates a new system prompt with:
- Task description
- Available tools
- Output variables to store

When these system messages were stored in memory and passed to the next task, it caused confusion because the LLM saw instructions for multiple different tasks mixed together.

The fix ensures only `user` and `assistant` messages (the actual conversation) are persisted across tasks, while each task gets a fresh system prompt built specifically for its requirements.

---

## Commands Used

```bash
# Build
cd /home/andreas/git/gh/deepclause-desktop/deepclause-sdk && npm run build

# Test
npx tsx examples/deep-research.ts "What is 2+2?" --debug-memory

# Check debug output
cat debug_memory.json | head -200
```

---

## Next Steps / Future Considerations

1. Consider whether tool call/result messages should also be formatted differently for better readability in debug output
2. The conversation mode after research completion could potentially benefit from the accumulated research context
3. May want to add option to control how much history is retained between tasks
