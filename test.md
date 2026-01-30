# Competitor Analysis

Analyze a company's competitors and generate a strategic report.

## Parameters

- `company` (required): The company name to analyze
- `depth` (optional, default: "standard"): Analysis depth (quick/standard/deep)

## Requirements

- Web search capability
- File writing for report output

## Behavior

1. Search for the company and identify its industry
2. Find top 5 competitors in that industry
3. For each competitor, gather:
   - Company overview
   - Key products/services
   - Strengths and weaknesses
4. Generate a comparative analysis
5. Save report to file

## Edge Cases

- If company not found, ask user for clarification
- If fewer than 5 competitors found, proceed with available data