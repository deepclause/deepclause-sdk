/**
 * Core types for the TUI v3 differential renderer.
 *
 * The component model is pull-based: components implement render(width) → string[],
 * and the framework only calls render when a component has been invalidated.
 */

/** A renderable component that produces rows of text. */
export interface Component {
  /** Render the component into an array of rows (one string per terminal line). */
  render(width: number): string[];

  /**
   * Mark this component as needing re-render.
   * Internally calls the event loop's requestRender().
   */
  invalidate(): void;

  /**
   * Handle a keypress event. Return true if the event was consumed.
   */
  handleInput?(key: KeyEvent): boolean;

  /**
   * Called when the component gains or loses focus.
   */
  onFocus?(focused: boolean): void;

  /**
   * Minimum height this component requires (0 = flexible).
   */
  minHeight?: number;

  /**
   * Flex grow factor for layout distribution (default 0 = fixed size).
   */
  flexGrow?: number;

  /**
   * Whether this component is currently dirty (needs re-render).
   */
  dirty: boolean;
}

/** Rectangle describing a component's position and size in the terminal. */
export interface LayoutRect {
  x: number;
  y: number;
  width: number;
  height: number;
}

/** A component that can receive focus. */
export interface Focusable extends Component {
  /** Whether this component currently has focus. */
  focused: boolean;
}

/** Parsed keypress event. */
export interface KeyEvent {
  /** Key name (e.g. 'return', 'up', 'a', 'tab') */
  name: string;
  /** The raw character sequence */
  sequence: string;
  /** Control key held */
  ctrl: boolean;
  /** Alt/Meta key held */
  meta: boolean;
  /** Shift key held */
  shift: boolean;
}

/** Mouse event from the terminal. */
export interface MouseEvent {
  button: 'left' | 'right' | 'middle' | 'wheel-up' | 'wheel-down' | 'release' | 'motion';
  row: number;
  col: number;
  shift: boolean;
  ctrl: boolean;
  meta: boolean;
}

/** Callback to schedule a render. */
export type RequestRender = () => void;

/** Cleanup function returned by lifecycle hooks. */
export type Cleanup = () => void;

/** Screen buffer: an array of string rows representing the full terminal. */
export type ScreenBuffer = string[];
