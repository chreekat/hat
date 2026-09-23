/* Scalar-only wrappers around libghostty-vt for the Haskell FFI: GHC can't pass
 * the tagged-union points, sized structs, or opaque cell handles by value, so
 * every one of those crossings happens here in C and only scalars (and one flat
 * GhostShimCell) reach Haskell. See "Hat.Term.Emulator". */
#include <stdlib.h>
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

/* Flatten a resolved cell and its style (NULL if unavailable) into *out. Shared
 * by the grid_ref path (ghost_shim_cell) and the render-state path
 * (ghost_shim_snapshot), so both decode a cell identically. Uses the style's
 * own colors, never the render state's resolved colors, so a palette index
 * stays an index. */
static void shim_from_cell(GhosttyCell cell, const GhosttyStyle *st,
                           GhostShimCell *out) {
    memset(out, 0, sizeof(*out));

    uint32_t cp = 0;
    ghostty_cell_get(cell, GHOSTTY_CELL_DATA_CODEPOINT, &cp);
    out->codepoint = cp;

    GhosttyCellContentTag content = GHOSTTY_CELL_CONTENT_CODEPOINT;
    ghostty_cell_get(cell, GHOSTTY_CELL_DATA_CONTENT_TAG, &content);
    out->grapheme = content == GHOSTTY_CELL_CONTENT_CODEPOINT_GRAPHEME;

    GhosttyCellWide wide = GHOSTTY_CELL_WIDE_NARROW;
    ghostty_cell_get(cell, GHOSTTY_CELL_DATA_WIDE, &wide);
    switch (wide) {
        case GHOSTTY_CELL_WIDE_WIDE:        out->width = 2; break;
        case GHOSTTY_CELL_WIDE_SPACER_TAIL: out->width = 0; break;
        case GHOSTTY_CELL_WIDE_SPACER_HEAD: out->width = 0; break;
        default:                            out->width = 1; break;
    }

    if (st) {
        unsigned f = 0;
        if (st->bold)          f |= 1;
        if (st->underline)     f |= 2;
        if (st->italic)        f |= 4;
        if (st->inverse)       f |= 8;
        if (st->strikethrough) f |= 16;
        if (st->blink)         f |= 32;
        if (st->faint)         f |= 64;
        out->flags = f;
        color_of(st->fg_color, &out->fg_tag, &out->fg_val);
        color_of(st->bg_color, &out->bg_tag, &out->bg_val);
    }

    /* A blank cell carrying only a background color stores it in the content,
     * not the style; surface that as the cell's bg. */
    if (content == GHOSTTY_CELL_CONTENT_BG_COLOR_PALETTE) {
        GhosttyColorPaletteIndex idx = 0;
        ghostty_cell_get(cell, GHOSTTY_CELL_DATA_COLOR_PALETTE, &idx);
        out->bg_tag = 1;
        out->bg_val = idx;
    } else if (content == GHOSTTY_CELL_CONTENT_BG_COLOR_RGB) {
        GhosttyColorRgb c = { 0 };
        ghostty_cell_get(cell, GHOSTTY_CELL_DATA_COLOR_RGB, &c);
        out->bg_tag = 2;
        out->bg_val = ((uint32_t)c.r << 16) | ((uint32_t)c.g << 8) | c.b;
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

    GhosttyStyle st = { .size = sizeof(GhosttyStyle) };
    const GhosttyStyle *stp =
        ghostty_grid_ref_style(&ref, &st) == GHOSTTY_SUCCESS ? &st : NULL;
    shim_from_cell(cell, stp, out);
    return 1;
}

/* One whole row's cells in a single page resolve: the node is looked up once
 * at column 0 and reused across the row — a row never spans pages, so the
 * ref's x is the terminal column. Failed rows come back all-blank. */
int ghost_shim_row_cells(void *t, int tag, uint32_t y, uint16_t cols,
                         GhostShimCell *out) {
    memset(out, 0, (size_t)cols * sizeof(*out));

    GhosttyPoint p = { .tag = (GhosttyPointTag)tag };
    p.value.coordinate.x = 0;
    p.value.coordinate.y = y;

    GhosttyGridRef ref = { .size = sizeof(GhosttyGridRef) };
    if (ghostty_terminal_grid_ref((GhosttyTerminal)t, p, &ref) != GHOSTTY_SUCCESS)
        return 0;
    for (uint16_t x = 0; x < cols; x++) {
        ref.x = x;
        GhosttyCell cell;
        if (ghostty_grid_ref_cell(&ref, &cell) != GHOSTTY_SUCCESS) {
            memset(out, 0, (size_t)cols * sizeof(*out));
            return 0;
        }
        GhosttyStyle st = { .size = sizeof(GhosttyStyle) };
        const GhosttyStyle *stp =
            ghostty_grid_ref_style(&ref, &st) == GHOSTTY_SUCCESS ? &st : NULL;
        shim_from_cell(cell, stp, &out[x]);
    }
    return 1;
}

/* The row painter's view of one cell: the paint-relevant fields, with the
 * style already flattened to the same words GhostShimCell carries. */
typedef struct {
    uint32_t cp;
    int      width;    /* 0 spacer, 1 narrow, 2 wide */
    int      grapheme;
    unsigned flags;
    int      fg_tag;
    uint32_t fg_val;
    int      bg_tag;
    uint32_t bg_val;
} PaintCell;

/* One resolved style, keyed by its id — valid within a single row (style ids
 * are page-local, and a row never spans pages). */
typedef struct {
    int      valid;
    uint16_t id;
    unsigned flags;
    int      fg_tag;
    uint32_t fg_val;
    int      bg_tag;
    uint32_t bg_val;
} StyleCache;

/* Read one cell's paint fields, resolving its style only when the style id
 * differs from the cached one. Same flattening as shim_from_cell. Returns 1
 * on success, 0 when the cell is unreadable. */
static int paint_cell(GhosttyGridRef *ref, StyleCache *sc, PaintCell *pc) {
    GhosttyCell cell;
    if (ghostty_grid_ref_cell(ref, &cell) != GHOSTTY_SUCCESS) return 0;

    uint32_t cp = 0;
    GhosttyCellContentTag content = GHOSTTY_CELL_CONTENT_CODEPOINT;
    GhosttyCellWide wide = GHOSTTY_CELL_WIDE_NARROW;
    uint16_t sid = 0;
    const GhosttyCellData keys[4] = {
        GHOSTTY_CELL_DATA_CODEPOINT, GHOSTTY_CELL_DATA_CONTENT_TAG,
        GHOSTTY_CELL_DATA_WIDE, GHOSTTY_CELL_DATA_STYLE_ID,
    };
    void *vals[4] = { &cp, &content, &wide, &sid };
    if (ghostty_cell_get_multi(cell, 4, keys, vals, NULL) != GHOSTTY_SUCCESS)
        return 0;

    pc->cp = cp;
    pc->grapheme = content == GHOSTTY_CELL_CONTENT_CODEPOINT_GRAPHEME;
    switch (wide) {
        case GHOSTTY_CELL_WIDE_WIDE:        pc->width = 2; break;
        case GHOSTTY_CELL_WIDE_SPACER_TAIL:
        case GHOSTTY_CELL_WIDE_SPACER_HEAD: pc->width = 0; break;
        default:                            pc->width = 1; break;
    }

    if (!sc->valid || sc->id != sid) {
        GhosttyStyle st = { .size = sizeof(GhosttyStyle) };
        sc->flags = 0;
        sc->fg_tag = 0; sc->fg_val = 0;
        sc->bg_tag = 0; sc->bg_val = 0;
        if (ghostty_grid_ref_style(ref, &st) == GHOSTTY_SUCCESS) {
            unsigned f = 0;
            if (st.bold)          f |= 1;
            if (st.underline)     f |= 2;
            if (st.italic)        f |= 4;
            if (st.inverse)       f |= 8;
            if (st.strikethrough) f |= 16;
            if (st.blink)         f |= 32;
            if (st.faint)         f |= 64;
            sc->flags = f;
            color_of(st.fg_color, &sc->fg_tag, &sc->fg_val);
            color_of(st.bg_color, &sc->bg_tag, &sc->bg_val);
        }
        sc->id = sid;
        sc->valid = 1;
    }
    pc->flags = sc->flags;
    pc->fg_tag = sc->fg_tag; pc->fg_val = sc->fg_val;
    pc->bg_tag = sc->bg_tag; pc->bg_val = sc->bg_val;

    /* A blank cell carrying only a background color stores it in the content,
     * not the style; surface that as the cell's bg. */
    if (content == GHOSTTY_CELL_CONTENT_BG_COLOR_PALETTE) {
        GhosttyColorPaletteIndex idx = 0;
        ghostty_cell_get(cell, GHOSTTY_CELL_DATA_COLOR_PALETTE, &idx);
        pc->bg_tag = 1;
        pc->bg_val = idx;
    } else if (content == GHOSTTY_CELL_CONTENT_BG_COLOR_RGB) {
        GhosttyColorRgb c = { 0 };
        ghostty_cell_get(cell, GHOSTTY_CELL_DATA_COLOR_RGB, &c);
        pc->bg_tag = 2;
        pc->bg_val = ((uint32_t)c.r << 16) | ((uint32_t)c.g << 8) | c.b;
    }
    return 1;
}

static int words_equal(const PaintCell *a, const PaintCell *b) {
    return a->flags == b->flags
        && a->fg_tag == b->fg_tag && a->fg_val == b->fg_val
        && a->bg_tag == b->bg_tag && a->bg_val == b->bg_val;
}

static int paint_blank(const PaintCell *pc, size_t nmarks) {
    return (pc->cp == 0 || pc->cp == 32) && pc->width == 1 && nmarks == 0
        && pc->flags == 0
        && pc->fg_tag != 1 && pc->fg_tag != 2
        && pc->bg_tag != 1 && pc->bg_tag != 2;
}

/* The combining codepoints of a cluster cell — the cluster minus its base —
 * retried once with the exact size when it outgrows the stack buffer. When
 * *heap is set the marks live there and the caller frees it. */
static size_t fetch_marks(const GhosttyGridRef *ref, uint32_t *stackbuf,
                          size_t stackcap, uint32_t **marks, uint32_t **heap) {
    size_t n = 0;
    GhosttyResult r = ghostty_grid_ref_graphemes(ref, stackbuf, stackcap, &n);
    uint32_t *buf = stackbuf;
    if (r == GHOSTTY_OUT_OF_SPACE && n > stackcap) {
        *heap = malloc(n * sizeof(uint32_t));
        if (*heap == NULL) return 0;
        buf = *heap;
        r = ghostty_grid_ref_graphemes(ref, buf, n, &n);
    }
    if (r != GHOSTTY_SUCCESS || n == 0) return 0;
    *marks = buf + 1;
    return n - 1;
}

static size_t put_str(uint8_t *out, size_t o, const char *s) {
    while (*s) out[o++] = (uint8_t)*s++;
    return o;
}

static size_t put_dec(uint8_t *out, size_t o, uint32_t v) {
    char tmp[10];
    int n = 0;
    do { tmp[n++] = (char)('0' + v % 10); v /= 10; } while (v);
    while (n) out[o++] = (uint8_t)tmp[--n];
    return o;
}

/* BB.stringUtf8-compatible for every code point, surrogates included (both
 * encode them by the plain 3-byte branch). */
static size_t put_utf8(uint8_t *out, size_t o, uint32_t u) {
    if (u < 0x80) {
        out[o++] = (uint8_t)u;
    } else if (u < 0x800) {
        out[o++] = (uint8_t)(0xc0 | (u >> 6));
        out[o++] = (uint8_t)(0x80 | (u & 0x3f));
    } else if (u < 0x10000) {
        out[o++] = (uint8_t)(0xe0 | (u >> 12));
        out[o++] = (uint8_t)(0x80 | ((u >> 6) & 0x3f));
        out[o++] = (uint8_t)(0x80 | (u & 0x3f));
    } else {
        out[o++] = (uint8_t)(0xf0 | (u >> 18));
        out[o++] = (uint8_t)(0x80 | ((u >> 12) & 0x3f));
        out[o++] = (uint8_t)(0x80 | ((u >> 6) & 0x3f));
        out[o++] = (uint8_t)(0x80 | (u & 0x3f));
    }
    return o;
}

/* One SGR color parameter: base is the 8-color offset (30 fg, 40 bg), ext the
 * 256/truecolor selector (38 fg, 48 bg). */
static size_t put_color(uint8_t *out, size_t o, unsigned base, unsigned ext,
                        int tag, uint32_t val) {
    if (tag == 1 && val < 8) {
        out[o++] = ';';
        o = put_dec(out, o, base + val);
    } else if (tag == 1) {
        out[o++] = ';';
        o = put_dec(out, o, ext);
        o = put_str(out, o, ";5;");
        o = put_dec(out, o, val);
    } else if (tag == 2) {
        out[o++] = ';';
        o = put_dec(out, o, ext);
        o = put_str(out, o, ";2;");
        o = put_dec(out, o, (val >> 16) & 0xff);
        out[o++] = ';';
        o = put_dec(out, o, (val >> 8) & 0xff);
        out[o++] = ';';
        o = put_dec(out, o, val & 0xff);
    }
    return o;
}

/* cellSgr's bytes: absolute SGR, reset then set, so each run stands alone. */
static size_t put_sgr(uint8_t *out, size_t o, const PaintCell *w) {
    o = put_str(out, o, "\x1b[0");
    if (w->flags & 1)  o = put_str(out, o, ";1");
    if (w->flags & 64) o = put_str(out, o, ";2");
    if (w->flags & 4)  o = put_str(out, o, ";3");
    if (w->flags & 2)  o = put_str(out, o, ";4");
    if (w->flags & 32) o = put_str(out, o, ";5");
    if (w->flags & 8)  o = put_str(out, o, ";7");
    if (w->flags & 16) o = put_str(out, o, ";9");
    o = put_color(out, o, 30, 38, w->fg_tag, w->fg_val);
    o = put_color(out, o, 40, 48, w->bg_tag, w->bg_val);
    out[o++] = 'm';
    return o;
}

long ghost_shim_paint_row(void *t, int tag, uint32_t y, uint16_t cols,
                          uint8_t *out, size_t cap) {
    GhosttyPoint p = { .tag = (GhosttyPointTag)tag };
    p.value.coordinate.x = 0;
    p.value.coordinate.y = y;
    GhosttyGridRef ref = { .size = sizeof(GhosttyGridRef) };
    if (ghostty_terminal_grid_ref((GhosttyTerminal)t, p, &ref) != GHOSTTY_SUCCESS)
        return 0;

    StyleCache sc = { 0 };
    uint32_t markbuf[64];
    uint32_t *heap = NULL;

    /* The last column whose cell must be painted. */
    int end = cols - 1;
    for (; end >= 0; end--) {
        ref.x = (uint16_t)end;
        PaintCell pc;
        if (!paint_cell(&ref, &sc, &pc)) return 0;
        size_t nmarks = 0;
        uint32_t *marks = NULL;
        if (pc.grapheme) {
            nmarks = fetch_marks(&ref, markbuf, 64, &marks, &heap);
            if (heap) { free(heap); heap = NULL; }
        }
        if (!paint_blank(&pc, nmarks)) break;
    }

    size_t o = 0;
    /* prev = the previous cell's raw style words; pen = the last style the
     * output established. Both start at the default pen, all zeros. */
    PaintCell prev = { 0 };
    PaintCell pen  = { 0 };
    for (int i = 0; i <= end; ) {
        ref.x = (uint16_t)i;
        PaintCell pc;
        if (!paint_cell(&ref, &sc, &pc)) return 0;

        size_t nmarks = 0;
        uint32_t *marks = NULL;
        if (pc.width != 0 && pc.grapheme)
            nmarks = fetch_marks(&ref, markbuf, 64, &marks, &heap);

        if (o + 68 + 4 * nmarks > cap) {
            if (heap) free(heap);
            return -1;
        }

        if (!words_equal(&pc, &prev)) {
            if (!words_equal(&pc, &pen)) o = put_sgr(out, o, &pc);
            pen = pc;
        }
        prev = pc;

        if (pc.width != 0) {
            o = put_utf8(out, o, pc.cp == 0 ? 0x20 : pc.cp);
            for (size_t m = 0; m < nmarks; m++) o = put_utf8(out, o, marks[m]);
        }
        if (heap) { free(heap); heap = NULL; }
        i += 1 + (pc.width >= 2 ? 1 : 0);
    }
    return (long)o;
}

long ghost_shim_format_history(void *t, uint32_t from, uint32_t to,
                               uint16_t cols, uint8_t **out) {
    GhosttyPoint a = { .tag = GHOSTTY_POINT_TAG_HISTORY };
    a.value.coordinate.x = 0;
    a.value.coordinate.y = from;
    GhosttyPoint b = { .tag = GHOSTTY_POINT_TAG_HISTORY };
    b.value.coordinate.x = cols ? cols - 1 : 0;
    b.value.coordinate.y = to;
    GhosttyGridRef ra = { .size = sizeof(GhosttyGridRef) };
    GhosttyGridRef rb = { .size = sizeof(GhosttyGridRef) };
    if (ghostty_terminal_grid_ref((GhosttyTerminal)t, a, &ra) != GHOSTTY_SUCCESS
     || ghostty_terminal_grid_ref((GhosttyTerminal)t, b, &rb) != GHOSTTY_SUCCESS)
        return -1;
    GhosttySelection sel = { .size = sizeof(GhosttySelection),
                             .start = ra, .end = rb, .rectangle = false };
    GhosttyFormatterTerminalOptions fo = { .size = sizeof fo };
    fo.emit = GHOSTTY_FORMATTER_FORMAT_VT;
    fo.trim = true;
    fo.extra.size = sizeof fo.extra;
    fo.extra.screen.size = sizeof fo.extra.screen;
    fo.selection = &sel;
    GhosttyFormatter f = NULL;
    if (ghostty_formatter_terminal_new(NULL, &f, (GhosttyTerminal)t, fo)
            != GHOSTTY_SUCCESS)
        return -1;
    uint8_t *buf = NULL;
    size_t len = 0;
    GhosttyResult r = ghostty_formatter_format_alloc(f, NULL, &buf, &len);
    ghostty_formatter_free(f);
    if (r != GHOSTTY_SUCCESS) return -1;
    *out = buf;
    return (long)len;
}

void ghost_shim_format_release(uint8_t *buf, size_t len) {
    ghostty_free(NULL, buf, len);
}

int ghost_shim_cell_graphemes(void *t, int tag, uint16_t x, uint32_t y,
                              uint32_t *buf, size_t buf_len, size_t *out_len) {
    GhosttyPoint p = { .tag = (GhosttyPointTag)tag };
    p.value.coordinate.x = x;
    p.value.coordinate.y = y;

    GhosttyGridRef ref = { .size = sizeof(GhosttyGridRef) };
    if (ghostty_terminal_grid_ref((GhosttyTerminal)t, p, &ref) != GHOSTTY_SUCCESS)
        return GHOSTTY_INVALID_VALUE;
    return ghostty_grid_ref_graphemes(&ref, buf, buf_len, out_len);
}

int ghost_shim_row_wrapped(void *t, int tag, uint32_t y) {
    GhosttyPoint p = { .tag = (GhosttyPointTag)tag };
    p.value.coordinate.x = 0;
    p.value.coordinate.y = y;

    GhosttyGridRef ref = { .size = sizeof(GhosttyGridRef) };
    if (ghostty_terminal_grid_ref((GhosttyTerminal)t, p, &ref) != GHOSTTY_SUCCESS)
        return 0;
    GhosttyRow row;
    if (ghostty_grid_ref_row(&ref, &row) != GHOSTTY_SUCCESS)
        return 0;
    bool wrapped = false;
    if (ghostty_row_get(row, GHOSTTY_ROW_DATA_WRAP, &wrapped) != GHOSTTY_SUCCESS)
        return 0;
    return wrapped ? 1 : 0;
}

/* The render-state trio, created once per terminal and reused every snapshot:
 * the state, plus a row iterator and a row-cells cursor to walk it. */
typedef struct {
    GhosttyRenderState rs;
    GhosttyRenderStateRowIterator iter;
    GhosttyRenderStateRowCells cells;
} GhostRender;

void *ghost_shim_render_new(void) {
    GhostRender *r = calloc(1, sizeof(*r));
    if (r == NULL) return NULL;
    if (ghostty_render_state_new(NULL, &r->rs) != GHOSTTY_SUCCESS)
        goto fail;
    if (ghostty_render_state_row_iterator_new(NULL, &r->iter) != GHOSTTY_SUCCESS)
        goto fail;
    if (ghostty_render_state_row_cells_new(NULL, &r->cells) != GHOSTTY_SUCCESS)
        goto fail;
    return r;
fail:
    ghost_shim_render_free(r);
    return NULL;
}

void ghost_shim_render_free(void *rp) {
    GhostRender *r = rp;
    if (r == NULL) return;
    if (r->cells != NULL) ghostty_render_state_row_cells_free(r->cells);
    if (r->iter != NULL) ghostty_render_state_row_iterator_free(r->iter);
    if (r->rs != NULL) ghostty_render_state_free(r->rs);
    free(r);
}

/* Update the render state from the terminal, then copy the viewport into out
 * (row-major, out[y*cols + x]) and set dirty[y] for each row changed since the
 * last snapshot. Resets the state's dirty tracking so the next snapshot reports
 * only fresh changes. Returns the number of rows written, 0 on failure. */
int ghost_shim_render_snapshot(void *rp, void *t, uint16_t cols, uint16_t rows,
                               GhostShimCell *out, uint8_t *dirty) {
    GhostRender *r = rp;
    if (r == NULL) return 0;
    if (ghostty_render_state_update(r->rs, (GhosttyTerminal)t) != GHOSTTY_SUCCESS)
        return 0;

    memset(out, 0, (size_t)rows * cols * sizeof(GhostShimCell));
    memset(dirty, 0, rows);

    if (ghostty_render_state_get(r->rs, GHOSTTY_RENDER_STATE_DATA_ROW_ITERATOR,
            &r->iter) != GHOSTTY_SUCCESS)
        return 0;

    uint16_t y = 0;
    while (y < rows && ghostty_render_state_row_iterator_next(r->iter)) {
        bool row_dirty = false;
        ghostty_render_state_row_get(r->iter,
            GHOSTTY_RENDER_STATE_ROW_DATA_DIRTY, &row_dirty);
        dirty[y] = row_dirty ? 1 : 0;

        if (ghostty_render_state_row_get(r->iter,
                GHOSTTY_RENDER_STATE_ROW_DATA_CELLS, &r->cells) == GHOSTTY_SUCCESS) {
            for (uint16_t x = 0; x < cols; x++) {
                if (ghostty_render_state_row_cells_select(r->cells, x)
                        != GHOSTTY_SUCCESS)
                    continue;
                GhosttyCell cell;
                if (ghostty_render_state_row_cells_get(r->cells,
                        GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_RAW, &cell)
                        != GHOSTTY_SUCCESS)
                    continue;
                GhosttyStyle st = { .size = sizeof(GhosttyStyle) };
                const GhosttyStyle *stp = ghostty_render_state_row_cells_get(
                    r->cells, GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_STYLE, &st)
                        == GHOSTTY_SUCCESS ? &st : NULL;
                shim_from_cell(cell, stp, &out[(size_t)y * cols + x]);
            }
        }

        bool clean = false;
        ghostty_render_state_row_set(r->iter,
            GHOSTTY_RENDER_STATE_ROW_OPTION_DIRTY, &clean);
        y++;
    }

    GhosttyRenderStateDirty none = GHOSTTY_RENDER_STATE_DIRTY_FALSE;
    ghostty_render_state_set(r->rs, GHOSTTY_RENDER_STATE_OPTION_DIRTY, &none);
    return y;
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

int ghost_shim_pen(void *t, GhostShimCell *out) {
    memset(out, 0, sizeof(*out));

    /* Emit the cursor's active SGR (plus one cell, which we discard): the
     * style extra is terminal-level, so restricting the content dump to a
     * one-cell selection keeps the pen without formatting the whole screen. */
    GhosttyFormatterTerminalOptions fo = { .size = sizeof(fo) };
    fo.emit = GHOSTTY_FORMATTER_FORMAT_VT;
    fo.extra.size = sizeof(fo.extra);
    fo.extra.screen.size = sizeof(fo.extra.screen);
    fo.extra.screen.style = true;
    GhosttyPoint origin = { .tag = GHOST_SHIM_ACTIVE };
    origin.value.coordinate.x = 0;
    origin.value.coordinate.y = 0;
    GhosttyGridRef oref = { .size = sizeof(GhosttyGridRef) };
    GhosttySelection sel = { .size = sizeof(GhosttySelection) };
    if (ghostty_terminal_grid_ref((GhosttyTerminal)t, origin, &oref)
            == GHOSTTY_SUCCESS) {
        sel.start = oref;
        sel.end = oref;
        fo.selection = &sel;
    }
    GhosttyFormatter f = NULL;
    if (ghostty_formatter_terminal_new(NULL, &f, (GhosttyTerminal)t, fo)
            != GHOSTTY_SUCCESS)
        return 0;
    uint8_t *buf = NULL;
    size_t len = 0;
    GhosttyResult fr = ghostty_formatter_format_alloc(f, NULL, &buf, &len);
    ghostty_formatter_free(f);
    if (fr != GHOSTTY_SUCCESS) return 0;

    /* Replay it into a scratch terminal so libghostty's own parser rebuilds the
     * pen, then take it off a space written under it. */
    GhosttyTerminal scratch = NULL;
    GhosttyTerminalOptions so = { .cols = 4, .rows = 2, .max_scrollback = 4096 };
    int ok = 0;
    if (ghostty_terminal_new(NULL, &scratch, so) == GHOSTTY_SUCCESS) {
        ghostty_terminal_vt_write(scratch, buf, len);
        ghostty_terminal_vt_write(scratch, (const uint8_t *)"\x1b[H ", 4);
        ok = ghost_shim_cell(scratch, GHOST_SHIM_ACTIVE, 0, 0, out);
        ghostty_terminal_free(scratch);
    }
    ghostty_free(NULL, buf, len);
    return ok;
}

/* The physical key an unshifted ASCII codepoint sits on. The encoder needs it
 * to produce a legacy encoding; a bare codepoint only carries under kitty. */
static GhosttyKey key_of_codepoint(uint32_t cp) {
    if (cp >= 'a' && cp <= 'z') return (GhosttyKey)(GHOSTTY_KEY_A + (cp - 'a'));
    if (cp >= '0' && cp <= '9')
        return (GhosttyKey)(GHOSTTY_KEY_DIGIT_0 + (cp - '0'));
    switch (cp) {
        case '`':  return GHOSTTY_KEY_BACKQUOTE;
        case '\\': return GHOSTTY_KEY_BACKSLASH;
        case '[':  return GHOSTTY_KEY_BRACKET_LEFT;
        case ']':  return GHOSTTY_KEY_BRACKET_RIGHT;
        case ',':  return GHOSTTY_KEY_COMMA;
        case '=':  return GHOSTTY_KEY_EQUAL;
        case '-':  return GHOSTTY_KEY_MINUS;
        case '.':  return GHOSTTY_KEY_PERIOD;
        case '\'': return GHOSTTY_KEY_QUOTE;
        case ';':  return GHOSTTY_KEY_SEMICOLON;
        case '/':  return GHOSTTY_KEY_SLASH;
        case ' ':  return GHOSTTY_KEY_SPACE;
        case 13:   return GHOSTTY_KEY_ENTER;
        case 9:    return GHOSTTY_KEY_TAB;
        case 27:   return GHOSTTY_KEY_ESCAPE;
        case 127:  return GHOSTTY_KEY_BACKSPACE;
        default:   return GHOSTTY_KEY_UNIDENTIFIED;
    }
}

static GhosttyMods mods_of(unsigned mods) {
    unsigned bits = mods > 0 ? mods - 1 : 0;
    GhosttyMods m = 0;
    if (bits & 1) m |= GHOSTTY_MODS_SHIFT;
    if (bits & 2) m |= GHOSTTY_MODS_ALT;
    if (bits & 4) m |= GHOSTTY_MODS_CTRL;
    return m;
}

static long encode_press(GhosttyKeyEncoder e, uint32_t cp, unsigned mods,
                         uint8_t *buf, size_t cap) {
    GhosttyKeyEvent ev = NULL;
    if (ghostty_key_event_new(NULL, &ev) != GHOSTTY_SUCCESS) return -1;
    ghostty_key_event_set_action(ev, GHOSTTY_KEY_ACTION_PRESS);
    ghostty_key_event_set_key(ev, key_of_codepoint(cp));
    ghostty_key_event_set_mods(ev, mods_of(mods));
    ghostty_key_event_set_unshifted_codepoint(ev, cp);
    /* The layout text the key produces; a C0 codepoint has none. */
    char text = (char)cp;
    if (cp >= 0x20 && cp < 0x7f) ghostty_key_event_set_utf8(ev, &text, 1);
    size_t n = 0;
    GhosttyResult r =
        ghostty_key_encoder_encode(e, ev, (char *)buf, cap, &n);
    ghostty_key_event_free(ev);
    return r == GHOSTTY_SUCCESS ? (long)n : -1;
}

/* Whether encoded bytes are an extended-key sequence: xterm's
 * CSI 27;mod;code~ or a CSI-u form. */
static int is_extended(const uint8_t *b, long n) {
    if (n < 3 || b[0] != 0x1b || b[1] != '[') return 0;
    if (n >= 5 && b[2] == '2' && b[3] == '7' && b[4] == ';') return 1;
    return b[n - 1] == 'u';
}

/* See ghost_shim_key_modes: alt+enter spells as an extended sequence only
 * under a key protocol. */
static int protocol_active(GhosttyKeyEncoder e) {
    uint8_t probe[64];
    long n = encode_press(e, 13, 3, probe, sizeof probe);
    return n > 0 && is_extended(probe, n);
}

long ghost_shim_encode_key(void *t, uint32_t cp, unsigned mods,
                           uint8_t *buf, size_t cap) {
    GhosttyKeyEncoder e = NULL;
    if (ghostty_key_encoder_new(NULL, &e) != GHOSTTY_SUCCESS) return -1;
    ghostty_key_encoder_setopt_from_terminal(e, (GhosttyTerminal)t);
    long n = encode_press(e, cp, mods, buf, cap);
    if (n > 0 && is_extended(buf, n) && !protocol_active(e))
        n = encode_press(e, cp, 1 + (((mods > 0 ? mods - 1 : 0)) & 2), buf, cap);
    ghostty_key_encoder_free(e);
    return n;
}

int ghost_shim_key_modes(void *t, uint8_t *kitty_flags) {
    GhosttyKittyKeyFlags kf = 0;
    if (ghostty_terminal_get((GhosttyTerminal)t,
            GHOSTTY_TERMINAL_DATA_KITTY_KEYBOARD_FLAGS, &kf) != GHOSTTY_SUCCESS)
        return -1;
    *kitty_flags = kf;
    GhosttyKeyEncoder e = NULL;
    if (ghostty_key_encoder_new(NULL, &e) != GHOSTTY_SUCCESS) return -1;
    ghostty_key_encoder_setopt_from_terminal(e, (GhosttyTerminal)t);
    int mok = kf == 0 && protocol_active(e);
    ghostty_key_encoder_free(e);
    return mok;
}
