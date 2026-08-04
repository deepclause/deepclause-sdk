/**
 * HStack layout — arranges child components horizontally.
 *
 * Each child gets a width allocation based on its flex properties.
 * Renders each child within its column allocation and joins them side-by-side.
 */
import { stripAnsi } from '../util/ansi.js';
export class HStack {
    dirty = true;
    minHeight = 1;
    flexGrow = 1;
    children = [];
    requestRenderFn;
    constructor(requestRender) {
        this.requestRenderFn = requestRender;
    }
    setChildren(children) {
        this.children = children;
        this.invalidate();
    }
    invalidate() {
        this.dirty = true;
        this.requestRenderFn();
    }
    render(width) {
        if (this.children.length === 0)
            return [''];
        const widths = this.allocateWidths(width);
        const columns = [];
        let maxHeight = 0;
        // Render each child with its allocated width
        for (let i = 0; i < this.children.length; i++) {
            const child = this.children[i];
            const childWidth = widths[i];
            const rows = child.component.render(childWidth);
            columns.push(rows);
            if (rows.length > maxHeight)
                maxHeight = rows.length;
            child.component.dirty = false;
        }
        // Join columns side-by-side
        const result = [];
        for (let row = 0; row < maxHeight; row++) {
            let line = '';
            for (let col = 0; col < columns.length; col++) {
                const cellContent = columns[col][row] ?? '';
                const cellWidth = widths[col];
                line += padRight(cellContent, cellWidth);
            }
            result.push(line);
        }
        this.dirty = false;
        return result;
    }
    /** Render with an explicit height constraint (returns first `height` rows). */
    renderWithHeight(width, height) {
        const rows = this.render(width);
        const result = [];
        for (let i = 0; i < height; i++) {
            result.push(rows[i] ?? '');
        }
        return result;
    }
    handleInput(key) {
        for (const child of this.children) {
            if (child.component.handleInput && child.component.handleInput(key)) {
                return true;
            }
        }
        return false;
    }
    allocateWidths(totalWidth) {
        const allocations = [];
        let fixedUsed = 0;
        let totalFlex = 0;
        for (const child of this.children) {
            if (child.width !== undefined) {
                allocations.push(child.width);
                fixedUsed += child.width;
            }
            else {
                allocations.push(0);
                totalFlex += child.flexGrow ?? 1;
            }
        }
        const remaining = Math.max(0, totalWidth - fixedUsed);
        if (remaining > 0 && totalFlex > 0) {
            let distributed = 0;
            for (let i = 0; i < this.children.length; i++) {
                if (this.children[i].width === undefined) {
                    const flex = this.children[i].flexGrow ?? 1;
                    const share = Math.floor((remaining * flex) / totalFlex);
                    allocations[i] = share;
                    distributed += share;
                }
            }
            // Give leftover to last flex child
            if (distributed < remaining) {
                for (let i = this.children.length - 1; i >= 0; i--) {
                    if (this.children[i].width === undefined) {
                        allocations[i] += remaining - distributed;
                        break;
                    }
                }
            }
        }
        return allocations;
    }
}
/** Pad a string with spaces to reach the desired visible width. */
function padRight(text, width) {
    const visible = stripAnsi(text).length;
    if (visible >= width)
        return text;
    return text + ' '.repeat(width - visible);
}
//# sourceMappingURL=hstack.js.map