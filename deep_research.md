# Deep Research Agent

Conduct comprehensive research on any topic and generate a detailed report.

Output should always cite sources and list source in the end.

## Parameters

- `question` (required): The topic or question to research
- `depth` (optional, default: "standard"): Research depth (quick/standard/deep)

## Requirements

- Web search capability
- File writing for report output

## Behavior

1. Analyze the research question to identify key aspects
2. Create a research plan and ask for confirmation if it's ok from the user before executing it.
3. Search for authoritative sources on each aspect
4. For each source, extract:
   - Key findings and facts
   - Supporting evidence
   - Expert opinions
5. Synthesize findings into a coherent analysis
6. Generate conclusions and recommendations
7. Save report to file

## Edge Cases

- If topic is ambiguous, ask user for clarification
- If limited sources found, note confidence level in report
- For controversial topics, present multiple perspectives