#ifndef GHOSTTY_SHIM_H
#define GHOSTTY_SHIM_H

#include <stddef.h>
#include <stdint.h>

/* One libghostty-vt cell flattened for the Haskell FFI: no tagged unions, no
 * accessor calls on the Haskell side. The flags bitmask matches hat_shim's
 * HatCell so the two backends decode identically:
 *   1 bold, 2 underline, 4 italic, 8 reverse, 16 strike, 32 blink, 64 faint.
 * Color tag: 0 default/none, 1 palette, 2 rgb; val is the palette index or a
 * packed 0xRRGGBB. width: 0 = wide-char continuation (spacer), 1, or 2. */
typedef struct {
    uint32_t codepoint; /* base codepoint, 0 = blank */
    int      width;
    unsigned flags;
    int      fg_tag;
    uint32_t fg_val;
    int      bg_tag;
    uint32_t bg_val;
} GhostShimCell;

/* Point tags mirrored from GhosttyPointTag for ghost_shim_cell. */
#define GHOST_SHIM_ACTIVE   0
#define GHOST_SHIM_VIEWPORT 1
#define GHOST_SHIM_HISTORY  3

void *ghost_shim_new(uint16_t cols, uint16_t rows, size_t max_scrollback);
void  ghost_shim_free(void *t);
void  ghost_shim_write(void *t, const uint8_t *data, size_t len);
void  ghost_shim_resize(void *t, uint16_t cols, uint16_t rows);

/* Read a scalar GhosttyTerminalData value (cursor x/y/visible, cols, rows,
 * scrollback rows, active screen). Returns the value, or -1 on failure. */
long  ghost_shim_get(void *t, int data);

/* Whether a DEC/ANSI mode is set (see ghostty_mode_new): ansi != 0 selects an
 * ANSI mode, otherwise a DEC private mode. Returns 1 set, 0 unset/failure. */
int   ghost_shim_mode(void *t, uint16_t mode_num, int ansi);

/* Read the cell at (x, y) under a point tag into *out. Returns 1 on success,
 * 0 when the ref or cell is unavailable (out is left zeroed). */
int   ghost_shim_cell(void *t, int tag, uint16_t x, uint32_t y, GhostShimCell *out);

/* Copy the terminal title (DATA_TITLE, set by OSC 0/2) into buf, truncated to
 * buflen bytes. Returns the full title length, or -1 on failure. */
long  ghost_shim_get_title(void *t, uint8_t *buf, size_t buflen);

/* Read the terminal's current pen (the style the next glyph would take) into
 * out's style fields. libghostty exposes the pen only as an SGR sequence from
 * the formatter, so it is replayed into a scratch terminal and read back off a
 * space cell. Returns 1 on success, 0 on failure. */
int   ghost_shim_pen(void *t, GhostShimCell *out);

#endif
