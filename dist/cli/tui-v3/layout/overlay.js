/**
 * Overlay — ANSI-aware string composition for modals/dialogs.
 *
 * Renders an overlay component on top of a background screen buffer,
 * centering it (or placing at specified coordinates) without clearing
 * the entire screen.
 */
import { stripAnsi } from '../util/ansi.js';
/**
 * Compose an overlay on top of a background screen buffer.
 * The overlay replaces characters at the specified position.
 */
export function composeOverlay(background, overlayRows, overlayWidth, screenWidth, screenHeight, position = { centerX: true, centerY: true }) {
    if (overlayRows.length === 0)
        return background;
    const overlayHeight = overlayRows.length;
    // Calculate position
    let startX;
    let startY;
    if (position.x !== undefined) {
        startX = position.x;
    }
    else if (position.centerX) {
        startX = Math.max(0, Math.floor((screenWidth - overlayWidth) / 2));
    }
    else {
        startX = 0;
    }
    if (position.y !== undefined) {
        startY = position.y;
    }
    else if (position.centerY) {
        startY = Math.max(0, Math.floor((screenHeight - overlayHeight) / 2));
    }
    else {
        startY = 0;
    }
    // Compose
    const result = [];
    for (let row = 0; row < screenHeight; row++) {
        const bgRow = background[row] ?? '';
        if (row >= startY && row < startY + overlayHeight) {
            const overlayLine = overlayRows[row - startY] ?? '';
            result.push(spliceRow(bgRow, overlayLine, startX, overlayWidth));
        }
        else {
            result.push(bgRow);
        }
    }
    return result;
}
/**
 * Replace a section of a background row with overlay content.
 * Simple approach: pads background to start position, inserts overlay.
 */
function spliceRow(bgRow, overlayLine, startX, overlayWidth) {
    const bgStripped = stripAnsi(bgRow);
    const bgLen = bgStripped.length;
    // Build: [bg left] [overlay] [bg right]
    const left = bgLen > startX ? bgRow.slice(0, startX) : bgRow + ' '.repeat(startX - bgLen);
    const rightStart = startX + overlayWidth;
    const right = bgLen > rightStart ? ' '.repeat(Math.max(0, rightStart - bgLen)) + bgStripped.slice(rightStart) : '';
    // Pad overlay to its width
    const overlayStrippedLen = stripAnsi(overlayLine).length;
    const paddedOverlay = overlayStrippedLen < overlayWidth
        ? overlayLine + ' '.repeat(overlayWidth - overlayStrippedLen)
        : overlayLine;
    return left + paddedOverlay + right;
}
//# sourceMappingURL=overlay.js.map