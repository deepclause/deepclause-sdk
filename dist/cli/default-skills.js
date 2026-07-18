import { createHash } from 'crypto';
import * as fs from 'fs/promises';
import * as path from 'path';
const RESEARCH_SEARCH_READER_DML = `% Deep research helper skill that turns raw search results into readable notes.

tool(search_web(Query, Results),
     "Search the web for background sources and evergreen references.") :-
    exec(web_search(query: Query, count: 8), Results).

tool(search_news(Query, Results),
     "Search recent news when the topic is time-sensitive or fast-moving.") :-
    exec(news_search(query: Query, count: 5), Results).

agent_main(Query) :-
    system("You turn raw search results into readable research notes.
- Always call search_web.
- Also call search_news when the query is time-sensitive, recent, or news-driven.
- Convert the raw results into concise Markdown that a parent research skill can reuse.
- Under Sources, list each source with its title, URL, and a one-line reason it matters.
- Do not invent facts. If the search results are sparse or conflicting, say so clearly."),

    output("Gathering search results..."),
    task("For the query '{Query}':
1. Call search_web.
2. Call search_news as well if recency matters.
3. Convert the raw search results into a readable Markdown brief with these sections:
   - Overview
   - Key Findings
   - Sources
Under Sources, list each source with its title, URL, and one-line relevance note.
Store the final brief in SearchBrief.", SearchBrief),

    answer(SearchBrief).
`;
const DEEP_RESEARCH_DML = `% Deep research skill that builds a research plan, confirms it, and writes a cited report.
% Adapted from the platform default deep-research example, but delegates searching to a child skill.

tool(search_topic(Query, Summary),
     "Delegate a focused search to the research-search-reader child skill and return a readable digest.") :-
    exec(run_skill(slug: "research-search-reader", args: [Query]), Raw),
    ( get_dict(answer, Raw, Answer) ->
        Summary = Answer
    ; get_dict(error, Raw, Error) ->
        format(string(Summary), "Search helper failed: ~w", [Error])
    ; Summary = "Search helper returned no final answer."
    ).

tool(ask_user(Prompt, Response), "Ask the user for clarification or plan approval.") :-
    exec(ask_user(prompt: Prompt), Result),
    get_dict(user_response, Result, Response).

agent_main(Question) :-
    system("You are a meticulous research assistant. Your goal is to conduct comprehensive research, synthesize findings, and generate a detailed report with citations.
- If the user's question is ambiguous, you must ask for clarification using the ask_user tool.
- Build a clear research plan before doing the full investigation.
- Use the search_topic tool to gather readable research notes with source links.
- For controversial topics, present multiple viewpoints.
- If you find limited sources, state this and describe your confidence level.
- Always cite sources inline and include a Sources section at the end."),

    output("Step 1: Analyzing the research question..."),
    task("Analyze the user's question to identify key topics and sub-questions for research. Question: '{Question}'. Store the key topics as a formatted list in KeyTopics.", KeyTopics),

    output("Step 2: Creating a research plan..."),
    task("Based on these key topics, create a step-by-step research plan. Topics: {KeyTopics}. The plan should be clear and concise. Store the plan as a formatted string in ResearchPlan.", ResearchPlan),

    output("Step 3: Confirming the plan with the user..."),
    confirm_plan(ResearchPlan),

    output("Step 4: Executing research and generating the report..."),
    task("Execute the approved research plan: {ResearchPlan}.
For each topic or sub-question, call search_topic to gather readable research notes with source links.
Synthesize the returned digests into a coherent, well-structured Markdown report.
The report must include inline citations and a Sources section at the end.
Store the final, complete report in FinalReport.", FinalReport),

    output("Step 5: Saving the report to a file..."),
    generate_filename(Question, Filename),
    open(Filename, write, Stream),
    write(Stream, FinalReport),
    close(Stream),

    format(string(AnswerMsg), "Research complete. Report saved to ~s.", [Filename]),
    answer(AnswerMsg).

confirm_plan(Plan) :-
    task("Present this research plan to the user and get their approval:

{Plan}

Use the ask_user tool to present the plan and ask if they approve.
- If they approve (yes/ok/looks good/etc.), set Approved to 'yes'.
- If they want changes, use ask_user to clarify what changes they want, then revise the plan and ask again. Continue this dialog until they approve.
- If they want to cancel entirely, set Approved to 'cancelled'.

Store 'yes' or 'cancelled' in Approved.", Approved),
    ( Approved == "yes" ->
        output("Plan approved. Proceeding with research.")
    ;
        answer("Research cancelled by user."), !, fail
    ).

generate_filename(Question, Filename) :-
    task("Generate a short, file-safe slug from this question: '{Question}'. For example, 'What is the future of AI?' could become 'future_of_ai'. Use underscores instead of spaces. Store only the slug in Slug.", Slug),
    atom_string(SlugAtom, Slug),
    atom_concat('report_', SlugAtom, BaseName),
    atom_concat(BaseName, '.md', FilenameAtom),
    atom_string(FilenameAtom, Filename).
`;
export async function writeDefaultSkillSeeds(toolsDir, modelId) {
    const seeds = getDefaultSkillSeeds(modelId);
    for (const seed of seeds) {
        await fs.writeFile(path.join(toolsDir, `${seed.slug}.dml`), seed.dml, 'utf8');
        await fs.writeFile(path.join(toolsDir, `${seed.slug}.meta.json`), JSON.stringify(seed.meta, null, 2) + '\n', 'utf8');
    }
}
export async function ensureDefaultSkillSeeds(toolsDir, modelId) {
    const seeds = getDefaultSkillSeeds(modelId);
    const written = [];
    await fs.mkdir(toolsDir, { recursive: true });
    for (const seed of seeds) {
        const dmlPath = path.join(toolsDir, `${seed.slug}.dml`);
        const metaPath = path.join(toolsDir, `${seed.slug}.meta.json`);
        const dmlExists = await pathExists(dmlPath);
        const metaExists = await pathExists(metaPath);
        if (dmlExists && metaExists) {
            continue;
        }
        if (!dmlExists) {
            await fs.writeFile(dmlPath, seed.dml, 'utf8');
        }
        if (!metaExists) {
            await fs.writeFile(metaPath, JSON.stringify(seed.meta, null, 2) + '\n', 'utf8');
        }
        written.push(seed.slug);
    }
    return written;
}
export function getDefaultSkillSeeds(modelId, compiledAt = new Date().toISOString()) {
    const provider = getProviderFromModelId(modelId);
    return [
        createSeed({
            slug: 'research-search-reader',
            description: 'Helper skill that wraps web and news search and rewrites raw results into readable research notes.',
            dml: RESEARCH_SEARCH_READER_DML,
            parameters: [
                { name: 'query', description: 'Focused search query to summarize', required: true, position: 0 },
            ],
            tools: ['news_search', 'web_search'],
        }, modelId, provider, compiledAt),
        createSeed({
            slug: 'deep-research',
            description: 'Conducts multi-source web research on any topic and saves a cited Markdown report to your workspace.',
            dml: DEEP_RESEARCH_DML,
            parameters: [
                { name: 'question', description: 'Research question or topic', required: true, position: 0 },
            ],
            tools: ['ask_user', 'run_skill'],
        }, modelId, provider, compiledAt),
    ];
}
function createSeed(seed, modelId, provider, compiledAt) {
    const dml = ensureTrailingNewline(seed.dml);
    const sourceHash = `sha256:${createHash('sha256').update(dml).digest('hex').slice(0, 16)}`;
    return {
        slug: seed.slug,
        dml,
        meta: {
            version: '1.0.0',
            source: `deepclause:init:${seed.slug}`,
            sourceHash,
            compiledAt,
            model: modelId,
            provider,
            description: seed.description,
            parameters: seed.parameters,
            tools: [...seed.tools],
            history: [
                {
                    version: 1,
                    timestamp: compiledAt,
                    sourceHash,
                    model: modelId,
                    provider,
                },
            ],
        },
    };
}
function getProviderFromModelId(modelId) {
    const [provider] = modelId.split(':', 1);
    return provider || 'openai';
}
async function pathExists(filePath) {
    try {
        await fs.access(filePath);
        return true;
    }
    catch {
        return false;
    }
}
function ensureTrailingNewline(value) {
    return value.endsWith('\n') ? value : `${value}\n`;
}
//# sourceMappingURL=default-skills.js.map