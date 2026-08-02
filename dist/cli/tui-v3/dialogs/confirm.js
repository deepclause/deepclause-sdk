/**
 * Confirm dialog — yes/no confirmation overlay.
 */
import { style, ANSI, center } from '../util/ansi.js';
export class ConfirmDialog {
    dirty = true;
    minHeight = 0;
    flexGrow = 0;
    requestRenderFn;
    visible = false;
    message = '';
    selectedYes = false;
    onConfirm = null;
    constructor(requestRender) {
        this.requestRenderFn = requestRender;
    }
    show(message) {
        this.message = message;
        this.selectedYes = false;
        this.visible = true;
        this.invalidate();
    }
    hide() {
        this.visible = false;
        this.invalidate();
    }
    setOnConfirm(fn) {
        this.onConfirm = fn;
    }
    get isVisible() {
        return this.visible;
    }
    invalidate() {
        this.dirty = true;
        this.requestRenderFn();
    }
    render(width) {
        if (!this.visible)
            return [];
        const maxWidth = Math.min(50, width - 4);
        const rows = [];
        const border = '─'.repeat(maxWidth - 2);
        rows.push(`┌${border}┐`);
        rows.push(`│${center(style('Confirm', ANSI.bold), maxWidth - 2)}│`);
        rows.push(`├${border}┤`);
        rows.push(`│${padLine('', maxWidth - 2)}│`);
        rows.push(`│${center(this.message, maxWidth - 2)}│`);
        rows.push(`│${padLine('', maxWidth - 2)}│`);
        // Buttons
        const yesBtn = this.selectedYes
            ? style(' [Yes] ', ANSI.bold, ANSI.inverse)
            : style(' [Yes] ', ANSI.dim);
        const noBtn = !this.selectedYes
            ? style(' [No] ', ANSI.bold, ANSI.inverse)
            : style(' [No] ', ANSI.dim);
        const buttons = `${yesBtn}   ${noBtn}`;
        rows.push(`│${center(buttons, maxWidth - 2)}│`);
        rows.push(`│${padLine('', maxWidth - 2)}│`);
        rows.push(`└${border}┘`);
        this.dirty = false;
        return rows;
    }
    handleInput(key) {
        if (!this.visible)
            return false;
        if (key.name === 'left' || key.name === 'right' || key.name === 'tab') {
            this.selectedYes = !this.selectedYes;
            this.invalidate();
            return true;
        }
        if (key.name === 'return') {
            this.hide();
            if (this.onConfirm)
                this.onConfirm(this.selectedYes);
            return true;
        }
        if (key.name === 'escape' || key.sequence === 'n') {
            this.hide();
            if (this.onConfirm)
                this.onConfirm(false);
            return true;
        }
        if (key.sequence === 'y') {
            this.hide();
            if (this.onConfirm)
                this.onConfirm(true);
            return true;
        }
        return true;
    }
}
function padLine(text, width) {
    const stripped = text.replace(/\u001b\[[0-9;]*[A-Za-z]/g, '');
    const padding = Math.max(0, width - stripped.length);
    return text + ' '.repeat(padding);
}
//# sourceMappingURL=confirm.js.map