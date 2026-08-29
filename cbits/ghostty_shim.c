/* Scalar-only wrappers around libghostty-vt for the Haskell FFI: GHC can't pass
 * the tagged-union points, sized structs, or opaque cell handles by value, so
 * every one of those crossings happens here in C and only scalars (and one flat
 * GhostShimCell) reach Haskell. See "Hat.Term.Emulator". */
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

    /* Emit the cursor's active SGR (plus the screen, which we discard). */
    GhosttyFormatterTerminalOptions fo = { .size = sizeof(fo) };
    fo.emit = GHOSTTY_FORMATTER_FORMAT_VT;
    fo.extra.size = sizeof(fo.extra);
    fo.extra.screen.size = sizeof(fo.extra.screen);
    fo.extra.screen.style = true;
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
