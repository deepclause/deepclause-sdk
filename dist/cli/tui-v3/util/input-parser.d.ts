/**
 * Input parser — converts raw terminal keypress data into structured KeyEvent objects.
 * Also handles mouse event parsing for SGR (1006) mouse mode.
 */
import type { KeyEvent, MouseEvent } from '../types.js';
/**
 * Parse a raw keypress into a KeyEvent.
 * Node's readline module already does most of the parsing; this normalizes the result.
 */
export declare function parseKeypress(ch: string | undefined, key: {
    name?: string;
    sequence?: string;
    ctrl?: boolean;
    meta?: boolean;
    shift?: boolean;
} | undefined): KeyEvent | null;
/**
 * Try to parse an SGR mouse event from a raw sequence.
 * SGR format: ESC [ < Cb ; Cx ; Cy M/m
 */
export declare function parseMouseEvent(sequence: string): MouseEvent | null;
//# sourceMappingURL=input-parser.d.ts.map