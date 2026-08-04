import { inspect } from 'util';
const originalConsole = {
    log: console.log,
    warn: console.warn,
    error: console.error,
};
const sinks = [];
let installed = false;
export async function withCapturedConsole(sink, run) {
    installConsoleCapture();
    sinks.push(sink);
    try {
        return await run();
    }
    finally {
        const index = sinks.lastIndexOf(sink);
        if (index >= 0) {
            sinks.splice(index, 1);
        }
        if (sinks.length === 0) {
            uninstallConsoleCapture();
        }
    }
}
function installConsoleCapture() {
    if (installed) {
        return;
    }
    console.log = (...args) => dispatch('log', args);
    console.warn = (...args) => dispatch('warn', args);
    console.error = (...args) => dispatch('error', args);
    installed = true;
}
function uninstallConsoleCapture() {
    if (!installed) {
        return;
    }
    console.log = originalConsole.log;
    console.warn = originalConsole.warn;
    console.error = originalConsole.error;
    installed = false;
}
function dispatch(level, args) {
    const sink = sinks[sinks.length - 1];
    if (!sink) {
        originalConsole[level](...args);
        return;
    }
    sink({
        level,
        text: formatConsoleArgs(args),
    });
}
function formatConsoleArgs(args) {
    return args.map((arg) => {
        if (typeof arg === 'string') {
            return arg;
        }
        return inspect(arg, {
            colors: false,
            depth: 5,
            breakLength: Infinity,
            maxArrayLength: 50,
        });
    }).join(' ');
}
//# sourceMappingURL=console-capture.js.map