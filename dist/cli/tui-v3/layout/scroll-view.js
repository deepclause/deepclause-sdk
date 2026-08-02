/**
 * ScrollView — a vertically scrolling container that shows a window
 * into its child's content. Supports follow-end mode for streaming content.
 */
export class ScrollView {
    dirty = true;
    minHeight = 1;
    flexGrow = 1;
    child;
    requestRenderFn;
    scrollOffset = 0;
    viewHeight = 0;
    contentHeight = 0;
    followEnd = true;
    constructor(child, requestRender) {
        this.child = child;
        this.requestRenderFn = requestRender;
    }
    /** Whether auto-scrolling to the end is active. */
    get isFollowing() {
        return this.followEnd;
    }
    /** Set follow-end mode. */
    setFollow(follow) {
        this.followEnd = follow;
        if (follow) {
            this.scrollToEnd();
        }
    }
    /** Toggle follow-end mode. */
    toggleFollow() {
        this.setFollow(!this.followEnd);
    }
    /** Scroll to the bottom. */
    scrollToEnd() {
        if (this.contentHeight > this.viewHeight) {
            this.scrollOffset = this.contentHeight - this.viewHeight;
        }
        else {
            this.scrollOffset = 0;
        }
        this.invalidate();
    }
    /** Scroll up by n lines. */
    scrollUp(n = 1) {
        this.followEnd = false;
        this.scrollOffset = Math.max(0, this.scrollOffset - n);
        this.invalidate();
    }
    /** Scroll down by n lines. */
    scrollDown(n = 1) {
        const maxOffset = Math.max(0, this.contentHeight - this.viewHeight);
        this.scrollOffset = Math.min(maxOffset, this.scrollOffset + n);
        if (this.scrollOffset >= maxOffset) {
            this.followEnd = true;
        }
        this.invalidate();
    }
    /** Page up. */
    pageUp() {
        this.scrollUp(Math.max(1, this.viewHeight - 1));
    }
    /** Page down. */
    pageDown() {
        this.scrollDown(Math.max(1, this.viewHeight - 1));
    }
    invalidate() {
        this.dirty = true;
        this.requestRenderFn();
    }
    render(width) {
        // Render the full child content
        const allRows = this.child.render(width);
        this.contentHeight = allRows.length;
        // The view height is set externally by the layout parent
        // For now, return all visible rows from the scroll offset
        return this.getVisibleRows(allRows);
    }
    /** Render with explicit height (used by layout). */
    renderWithHeight(width, height) {
        this.viewHeight = height;
        const allRows = this.child.render(width);
        this.contentHeight = allRows.length;
        // Auto-follow: scroll to end if follow mode is active
        if (this.followEnd && this.contentHeight > height) {
            this.scrollOffset = this.contentHeight - height;
        }
        // Clamp scroll offset
        const maxOffset = Math.max(0, this.contentHeight - height);
        if (this.scrollOffset > maxOffset) {
            this.scrollOffset = maxOffset;
        }
        const visible = this.getVisibleRows(allRows);
        // Pad to fill the view height
        const result = [];
        for (let i = 0; i < height; i++) {
            result.push(visible[i] ?? '');
        }
        this.child.dirty = false;
        this.dirty = false;
        return result;
    }
    handleInput(key) {
        // Handle scroll keys
        if (key.name === 'up' && key.shift) {
            this.scrollUp(1);
            return true;
        }
        if (key.name === 'down' && key.shift) {
            this.scrollDown(1);
            return true;
        }
        if (key.name === 'pageup' || (key.name === 'u' && key.ctrl)) {
            this.pageUp();
            return true;
        }
        if (key.name === 'pagedown' || (key.name === 'd' && key.ctrl)) {
            this.pageDown();
            return true;
        }
        if (key.name === 'home') {
            this.scrollOffset = 0;
            this.followEnd = false;
            this.invalidate();
            return true;
        }
        if (key.name === 'end') {
            this.setFollow(true);
            return true;
        }
        // Delegate to child
        if (this.child.handleInput) {
            return this.child.handleInput(key);
        }
        return false;
    }
    getVisibleRows(allRows) {
        const start = Math.max(0, Math.min(this.scrollOffset, allRows.length));
        const end = start + this.viewHeight;
        return allRows.slice(start, end);
    }
}
//# sourceMappingURL=scroll-view.js.map