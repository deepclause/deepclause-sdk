export function buildDmlParams(args, namedParams, meta) {
    const params = {};
    if (args.length > 0) {
        params.args = args.map(parseDmlArgValue);
    }
    if (meta?.parameters && args.length > 0) {
        const sortedParams = [...meta.parameters].sort((left, right) => left.position - right.position);
        for (let index = 0; index < args.length && index < sortedParams.length; index += 1) {
            const param = sortedParams[index];
            params[param.name] = parseDmlArgValue(args[index]);
        }
        for (let index = sortedParams.length; index < args.length; index += 1) {
            params[`arg${index + 1}`] = parseDmlArgValue(args[index]);
        }
    }
    if (namedParams) {
        for (const [key, value] of Object.entries(namedParams)) {
            params[key] = parseDmlArgValue(value);
        }
    }
    return params;
}
export function parseDmlArgValue(value) {
    if (typeof value !== 'string') {
        return value;
    }
    const num = Number(value);
    if (!Number.isNaN(num) && value.trim() !== '') {
        return num;
    }
    if (value.toLowerCase() === 'true') {
        return true;
    }
    if (value.toLowerCase() === 'false') {
        return false;
    }
    if ((value.startsWith('{') && value.endsWith('}'))
        || (value.startsWith('[') && value.endsWith(']'))) {
        try {
            return JSON.parse(value);
        }
        catch {
            return value;
        }
    }
    return value;
}
//# sourceMappingURL=dml-params.js.map