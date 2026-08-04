/**
 * Overlay — ANSI-aware string composition for modals/dialogs.
 *
 * Renders an overlay component on top of a background screen buffer,
 * centering it (or placing at specified coordinates) without clearing
 * the entire screen.
 */
import type { ScreenBuffer } from '../types.js';
export interface OverlayPosition {
    /** Center the overlay horizontally */
    centerX?: boolean;
    /** Center the overlay vertically */
    centerY?: boolean;
    /** Fixed x position (overrides centerX) */
    x?: number;
    /** Fixed y position (overrides centerY) */
    y?: number;
}
/**
 * Compose an overlay on top of a background screen buffer.
 * The overlay replaces characters at the specified position.
 */
export declare function composeOverlay(background: ScreenBuffer, overlayRows: string[], overlayWidth: number, screenWidth: number, screenHeight: number, position?: OverlayPosition): ScreenBuffer;
//# sourceMappingURL=overlay.d.ts.map