/**
 * Sessions pane component — collapsible sidebar showing session list.
 * Default collapsed to icon-width (3 chars), expand to show full titles.
 */
import { style, ANSI, truncate } from '../util/ansi.js';
export class Sessions {
    dirty = true;
    minHeight = 1;
    flexGrow = 1;
    requestRenderFn;
    sessions = [];
    activeSessionId = null;
    selectedIndex = 0;
    collapsed = false;
    onSelect = null;
    constructor(requestRender) {
        this.requestRenderFn = requestRender;
    }
    /** Set the list of sessions. */
    setSessions(sessions) {
        const selectedId = this.sessions[this.selectedIndex]?.id;
        this.sessions = sessions;
        const selectedIndex = sessions.findIndex((session) => session.id === selectedId);
        const activeIndex = sessions.findIndex((session) => session.id === this.activeSessionId);
        if (selectedIndex >= 0) {
            this.selectedIndex = selectedIndex;
        }
        else if (activeIndex >= 0) {
            this.selectedIndex = activeIndex;
        }
        else {
            this.selectedIndex = Math.min(this.selectedIndex, Math.max(0, sessions.length - 1));
        }
        this.invalidate();
    }
    /** Set the active session. */
    setActiveSession(id) {
        if (this.activeSessionId === id)
            return;
        this.activeSessionId = id;
        const activeIndex = this.sessions.findIndex((session) => session.id === id);
        if (activeIndex >= 0)
            this.selectedIndex = activeIndex;
        this.invalidate();
    }
    setOnSelect(fn) {
        this.onSelect = fn;
    }
    /** Toggle collapsed state. */
    toggleCollapsed() {
        this.collapsed = !this.collapsed;
        this.invalidate();
    }
    /** Get collapsed state. */
    get isCollapsed() {
        return this.collapsed;
    }
    /** Get the display width. */
    get displayWidth() {
        return this.collapsed ? 3 : 30;
    }
    invalidate() {
        this.dirty = true;
        this.requestRenderFn();
    }
    render(width) {
        return this.renderWithHeight(width, Number.MAX_SAFE_INTEGER);
    }
    renderWithHeight(width, height) {
        const rows = [];
        if (this.collapsed) {
            // Collapsed: just show "S" header and numbered indicators
            rows.push(style('S', ANSI.cyan, ANSI.bold));
            for (let i = 0; i < this.sessions.length; i++) {
                const s = this.sessions[i];
                const color = s.id === this.activeSessionId ? ANSI.green : ANSI.dim;
                rows.push(style(String(i + 1), color));
            }
        }
        else {
            // Expanded: show full session list
            rows.push(style('Sessions', ANSI.bold));
            rows.push(style('─'.repeat(Math.min(width, 22)), ANSI.dim));
            if (this.sessions.length === 0) {
                rows.push(style('  No sessions', ANSI.dim));
            }
            else {
                const entryRows = this.sessions.map((session, index) => this.renderSession(session, index, width));
                const availableRows = Math.max(1, height - rows.length);
                let startIndex = 0;
                let rowsThroughSelection = entryRows
                    .slice(0, this.selectedIndex + 1)
                    .reduce((total, entry) => total + entry.length, 0);
                while (rowsThroughSelection > availableRows && startIndex < this.selectedIndex) {
                    rowsThroughSelection -= entryRows[startIndex].length;
                    startIndex++;
                }
                let usedRows = 0;
                for (let i = startIndex; i < entryRows.length; i++) {
                    if (usedRows + entryRows[i].length > availableRows)
                        break;
                    rows.push(...entryRows[i]);
                    usedRows += entryRows[i].length;
                }
            }
        }
        this.dirty = false;
        return rows;
    }
    handleInput(key) {
        if (this.sessions.length === 0)
            return false;
        if (key.name === 'up') {
            this.selectedIndex = Math.max(0, this.selectedIndex - 1);
            this.invalidate();
            return true;
        }
        if (key.name === 'down') {
            this.selectedIndex = Math.min(this.sessions.length - 1, this.selectedIndex + 1);
            this.invalidate();
            return true;
        }
        if (key.name === 'home') {
            this.selectedIndex = 0;
            this.invalidate();
            return true;
        }
        if (key.name === 'end') {
            this.selectedIndex = this.sessions.length - 1;
            this.invalidate();
            return true;
        }
        if (key.name === 'return') {
            const selected = this.sessions[this.selectedIndex];
            if (selected && selected.id !== this.activeSessionId)
                this.onSelect?.(selected.id);
            return true;
        }
        return false;
    }
    renderSession(session, index, width) {
        const rows = [];
        const isActive = session.id === this.activeSessionId;
        const isSelected = index === this.selectedIndex;
        const prefix = isSelected ? '▸ ' : isActive ? '● ' : '  ';
        const line = prefix + (session.title || session.id.slice(0, 8));
        if (isSelected) {
            rows.push(style(truncate(line, width), ANSI.cyan, ANSI.bold));
        }
        else if (isActive) {
            rows.push(style(truncate(line, width), ANSI.green, ANSI.bold));
        }
        else {
            rows.push(truncate(line, width));
        }
        if (session.updatedAt) {
            rows.push(style(truncate(`  ${formatSessionDate(session.updatedAt)}`, width), ANSI.dim));
        }
        return rows;
    }
}
function formatSessionDate(value) {
    const date = new Date(value);
    if (Number.isNaN(date.getTime()))
        return value.replace('T', ' ').slice(0, 16);
    return date.toLocaleString(undefined, {
        month: 'short',
        day: 'numeric',
        hour: '2-digit',
        minute: '2-digit',
    });
}
//# sourceMappingURL=sessions.js.map