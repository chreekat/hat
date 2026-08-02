/* Scalar-only wrappers around libghostty-vt for the Haskell FFI, mirroring how
 * hat_shim.c flattens libvterm: GHC can't pass the tagged-union points, sized
 * structs, or opaque cell handles by value, so every one of those crossings
 * happens here in C and only scalars (and one flat GhostShimCell) reach
 * Haskell. See "Hat.Term.Emulator" (the ghostty backend). */
#include <string.h>
#include <ghostty/vt.h>

#include "ghostty_shim.h"

void *ghost_shim_new(uint16_t cols, uint16_t rows, size_t max_scrollback) {
    GhosttyTerminal t = NULL;
    GhosttyTerminalOptions o = {
        .cols = cols,
        .rows = rows,
        .max_scrollback = max_scrollback,
    };
    if (ghostty_terminal_new(NULL, &t, o) != GHOSTTY_SUCCESS) return NULL;
    return t;
}

void ghost_shim_free(void *t) {
    ghostty_terminal_free((GhosttyTerminal)t);
}

void ghost_shim_write(void *t, const uint8_t *data, size_t len) {
    ghostty_terminal_vt_write((GhosttyTerminal)t, data, len);
}

void ghost_shim_resize(void *t, uint16_t cols, uint16_t rows) {
    /* Cell pixel size is irrelevant to hat (no image protocol); pass 1x1. */
    ghostty_terminal_resize((GhosttyTerminal)t, cols, rows, 1, 1);
}

long ghost_shim_get(void *t, int data) {
    /* The out type varies by data kind (bool, enum, size_t); a zeroed 64-bit
     * buffer holds any of them, and reading it back as a long is exact on
     * little-endian targets. */
    uint64_t buf = 0;
    if (ghostty_terminal_get((GhosttyTerminal)t, (GhosttyTerminalData)data, &buf)
            != GHOSTTY_SUCCESS)
        return -1;
    return (long)buf;
}

int ghost_shim_mode(void *t, uint16_t mode_num, int ansi) {
    bool v = false;
    if (ghostty_terminal_mode_get((GhosttyTerminal)t,
            ghostty_mode_new(mode_num, ansi != 0), &v) != GHOSTTY_SUCCESS)
        return 0;
    return v ? 1 : 0;
}

static void color_of(GhosttyStyleColor col, int *tag, uint32_t *val) {
    switch (col.tag) {
        case GHOSTTY_STYLE_COLOR_PALETTE:
            *tag = 1;
            *val = col.value.palette;
            break;
        case GHOSTTY_STYLE_COLOR_RGB: {
            GhosttyColorRgb c = col.value.rgb;
            *tag = 2;
            *val = ((uint32_t)c.r << 16) | ((uint32_t)c.g << 8) | c.b;
            break;
        }
        default:
            *tag = 0;
            *val = 0;
            break;
    }
}

int ghost_shim_cell(void *t, int tag, uint16_t x, uint32_t y, GhostShimCell *out) {
    memset(out, 0, sizeof(*out));

    GhosttyPoint p = { .tag = (GhosttyPointTag)tag };
    p.value.coordinate.x = x;
    p.value.coordinate.y = y;

    GhosttyGridRef ref = { .size = sizeof(GhosttyGridRef) };
    if (ghostty_terminal_grid_ref((GhosttyTerminal)t, p, &ref) != GHOSTTY_SUCCESS)
        return 0;
    GhosttyCell cell;
    if (ghostty_grid_ref_cell(&ref, &cell) != GHOSTTY_SUCCESS)
        return 0;

    uint32_t cp = 0;
    ghostty_cell_get(cell, GHOSTTY_CELL_DATA_CODEPOINT, &cp);
    out->codepoint = cp;

    GhosttyCellWide wide = GHOSTTY_CELL_WIDE_NARROW;
    ghostty_cell_get(cell, GHOSTTY_CELL_DATA_WIDE, &wide);
    switch (wide) {
        case GHOSTTY_CELL_WIDE_WIDE:        out->width = 2; break;
        case GHOSTTY_CELL_WIDE_SPACER_TAIL: out->width = 0; break;
        case GHOSTTY_CELL_WIDE_SPACER_HEAD: out->width = 0; break;
        default:                            out->width = 1; break;
    }

    GhosttyStyle st = { .size = sizeof(GhosttyStyle) };
    if (ghostty_grid_ref_style(&ref, &st) == GHOSTTY_SUCCESS) {
        unsigned f = 0;
        if (st.bold)          f |= 1;
        if (st.underline)     f |= 2;
        if (st.italic)        f |= 4;
        if (st.inverse)       f |= 8;
        if (st.strikethrough) f |= 16;
        if (st.blink)         f |= 32;
        if (st.faint)         f |= 64;
        out->flags = f;
        color_of(st.fg_color, &out->fg_tag, &out->fg_val);
        color_of(st.bg_color, &out->bg_tag, &out->bg_val);
    }

    /* A blank cell carrying only a background color stores it in the content,
     * not the style; surface that as the cell's bg. */
    GhosttyCellContentTag ctag = GHOSTTY_CELL_CONTENT_CODEPOINT;
    ghostty_cell_get(cell, GHOSTTY_CELL_DATA_CONTENT_TAG, &ctag);
    if (ctag == GHOSTTY_CELL_CONTENT_BG_COLOR_PALETTE) {
        GhosttyColorPaletteIndex idx = 0;
        ghostty_cell_get(cell, GHOSTTY_CELL_DATA_COLOR_PALETTE, &idx);
        out->bg_tag = 1;
        out->bg_val = idx;
    } else if (ctag == GHOSTTY_CELL_CONTENT_BG_COLOR_RGB) {
        GhosttyColorRgb c = { 0 };
        ghostty_cell_get(cell, GHOSTTY_CELL_DATA_COLOR_RGB, &c);
        out->bg_tag = 2;
        out->bg_val = ((uint32_t)c.r << 16) | ((uint32_t)c.g << 8) | c.b;
    }
    return 1;
}

long ghost_shim_get_title(void *t, uint8_t *buf, size_t buflen) {
    GhosttyString s = { 0 };
    if (ghostty_terminal_get((GhosttyTerminal)t, GHOSTTY_TERMINAL_DATA_TITLE, &s)
            != GHOSTTY_SUCCESS)
        return -1;
    size_t n = s.len < buflen ? s.len : buflen;
    if (s.ptr != NULL && n > 0) memcpy(buf, s.ptr, n);
    return (long)s.len;
}
