export function formatToolArgs(args, maxValueLength = 50) {
    if (!args) {
        return '';
    }
    const parts = [];
    for (const [key, value] of Object.entries(args)) {
        const rendered = renderToolArgValue(value);
        if (rendered === undefined) {
            continue;
        }
        parts.push(`${key}=${truncate(rendered, maxValueLength)}`);
    }
    return parts.join(', ');
}
function renderToolArgValue(value) {
    if (value === undefined) {
        return undefined;
    }
    if (typeof value === 'string') {
        return value;
    }
    try {
        const rendered = JSON.stringify(value);
        if (typeof rendered === 'string') {
            return rendered;
        }
    }
    catch {
        // Fall back to String(value) below.
    }
    return String(value);
}
function truncate(value, maxValueLength) {
    if (value.length <= maxValueLength) {
        return value;
    }
    return value.slice(0, Math.max(0, maxValueLength - 3)) + '...';
}
//# sourceMappingURL=tool-args.js.map