/**
 * Container — wraps a child with optional padding and border.
 */
import { stripAnsi } from '../util/ansi.js';
const DEFAULT_BORDER = {
    topLeft: '┌',
    topRight: '┐',
    bottomLeft: '└',
    bottomRight: '┘',
    horizontal: '─',
    vertical: '│',
};
export class Container {
    dirty = true;
    minHeight = 0;
    flexGrow = 0;
    child;
    requestRenderFn;
    options;
    constructor(child, requestRender, options = {}) {
        this.child = child;
        this.requestRenderFn = requestRender;
        this.options = options;
    }
    /** Update container options (e.g. title). */
    setOptions(options) {
        Object.assign(this.options, options);
        this.invalidate();
    }
    invalidate() {
        this.dirty = true;
        this.requestRenderFn();
    }
    render(width) {
        const { padding = [0, 0, 0, 0], border, borderChars = DEFAULT_BORDER, title } = this.options;
        const [pt, pr, pb, pl] = padding;
        const borderWidth = border ? 2 : 0; // left + right
        const innerWidth = Math.max(0, width - pl - pr - borderWidth);
        // Render child
        const childRows = this.child.render(innerWidth);
        this.child.dirty = false;
        const rows = [];
        // Top border
        if (border) {
            let topLine = borderChars.topLeft;
            if (title) {
                const titleStr = ` ${title} `;
                topLine += titleStr;
                topLine += borderChars.horizontal.repeat(Math.max(0, width - 2 - stripAnsi(titleStr).length));
            }
            else {
                topLine += borderChars.horizontal.repeat(Math.max(0, width - 2));
            }
            topLine += borderChars.topRight;
            rows.push(topLine);
        }
        // Top padding
        for (let i = 0; i < pt; i++) {
            rows.push(border
                ? borderChars.vertical + ' '.repeat(width - 2) + borderChars.vertical
                : ' '.repeat(width));
        }
        // Content rows
        for (const row of childRows) {
            const leftPad = ' '.repeat(pl);
            const rightPad = ' '.repeat(pr);
            if (border) {
                rows.push(borderChars.vertical + leftPad + padToWidth(row, innerWidth) + rightPad + borderChars.vertical);
            }
            else {
                rows.push(leftPad + row + rightPad);
            }
        }
        // Bottom padding
        for (let i = 0; i < pb; i++) {
            rows.push(border
                ? borderChars.vertical + ' '.repeat(width - 2) + borderChars.vertical
                : ' '.repeat(width));
        }
        // Bottom border
        if (border) {
            rows.push(borderChars.bottomLeft +
                borderChars.horizontal.repeat(Math.max(0, width - 2)) +
                borderChars.bottomRight);
        }
        this.dirty = false;
        return rows;
    }
    handleInput(key) {
        if (this.child.handleInput) {
            return this.child.handleInput(key);
        }
        return false;
    }
}
function padToWidth(text, width) {
    const visible = stripAnsi(text).length;
    if (visible >= width)
        return text;
    return text + ' '.repeat(width - visible);
}
//# sourceMappingURL=container.js.map