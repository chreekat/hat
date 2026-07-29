#define _GNU_SOURCE  /* execvpe */
#include "hat_shim.h"

#include <errno.h>
#include <stdbool.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>
#include <sys/ioctl.h>

static void flatten_color(const VTermColor *c, int *kind, int *idx,
                          int *r, int *g, int *b) {
    *idx = *r = *g = *b = 0;
    if (VTERM_COLOR_IS_DEFAULT_FG(c) || VTERM_COLOR_IS_DEFAULT_BG(c)) {
        *kind = 0;
    } else if (VTERM_COLOR_IS_INDEXED(c)) {
        *kind = 1;
        *idx = c->indexed.idx;
    } else {
        *kind = 2;
        *r = c->rgb.red;
        *g = c->rgb.green;
        *b = c->rgb.blue;
    }
}

static void flatten_cell(const VTermScreenCell *in, HatCell *out) {
    memset(out, 0, sizeof *out);
    for (int i = 0; i < VTERM_MAX_CHARS_PER_CELL; i++)
        out->chars[i] = in->chars[i];
    out->width = in->width;
    out->flags = (in->attrs.bold ? 1u : 0u)
               | (in->attrs.underline ? 2u : 0u)
               | (in->attrs.italic ? 4u : 0u)
               | (in->attrs.reverse ? 8u : 0u)
               | (in->attrs.strike ? 16u : 0u)
               | (in->attrs.blink ? 32u : 0u)
               | (in->attrs.dim ? 64u : 0u);
    flatten_color(&in->fg, &out->fg_kind, &out->fg_idx,
                  &out->fg_r, &out->fg_g, &out->fg_b);
    flatten_color(&in->bg, &out->bg_kind, &out->bg_idx,
                  &out->bg_r, &out->bg_g, &out->bg_b);
}

int hat_get_cell(VTermScreen *screen, int row, int col, HatCell *out) {
    VTermScreenCell cell;
    VTermPos pos;
    pos.row = row;
    pos.col = col;
    memset(&cell, 0, sizeof cell);
    int ok = vterm_screen_get_cell(screen, pos, &cell);
    flatten_cell(&cell, out);
    return ok;
}

void hat_flatten_cell_at(const VTermScreenCell *cells, int i, HatCell *out) {
    flatten_cell(cells + i, out);
}

static void unflatten_color(VTermColor *c, int kind, int idx,
                            int r, int g, int b, uint8_t default_flag) {
    if (kind == 1)
        vterm_color_indexed(c, idx);
    else if (kind == 2)
        vterm_color_rgb(c, r, g, b);
    else
        c->type = default_flag;
}

static void unflatten_cell(const HatCell *in, VTermScreenCell *out) {
    memset(out, 0, sizeof *out);
    for (int i = 0; i < VTERM_MAX_CHARS_PER_CELL; i++)
        out->chars[i] = in->chars[i];
    out->width = in->width;
    out->attrs.bold      = (in->flags & 1u)  ? 1 : 0;
    out->attrs.underline = (in->flags & 2u)  ? 1 : 0;
    out->attrs.italic    = (in->flags & 4u)  ? 1 : 0;
    out->attrs.reverse   = (in->flags & 8u)  ? 1 : 0;
    out->attrs.strike    = (in->flags & 16u) ? 1 : 0;
    out->attrs.blink     = (in->flags & 32u) ? 1 : 0;
    out->attrs.dim       = (in->flags & 64u) ? 1 : 0;
    unflatten_color(&out->fg, in->fg_kind, in->fg_idx,
                    in->fg_r, in->fg_g, in->fg_b, VTERM_COLOR_DEFAULT_FG);
    unflatten_color(&out->bg, in->bg_kind, in->bg_idx,
                    in->bg_r, in->bg_g, in->bg_b, VTERM_COLOR_DEFAULT_BG);
}

void hat_unflatten_cell_at(VTermScreenCell *cells, int i, const HatCell *in) {
    unflatten_cell(in, cells + i);
}

/* --- trampolines: vterm calls these with structs by value; we forward
 * scalars to the Haskell function pointers stashed in the cbdata. --- */

static int tramp_damage(VTermRect rect, void *user) {
    HatCallbacks *h = user;
    if (h->damage)
        h->damage(rect.start_row, rect.end_row, rect.start_col, rect.end_col);
    return 1;
}

static int tramp_movecursor(VTermPos pos, VTermPos oldpos, int visible, void *user) {
    (void)oldpos;
    HatCallbacks *h = user;
    if (h->movecursor)
        h->movecursor(pos.row, pos.col, visible);
    return 1;
}

static int tramp_settermprop(VTermProp prop, VTermValue *val, void *user) {
    HatCallbacks *h = user;
    switch (vterm_get_prop_type(prop)) {
    case VTERM_VALUETYPE_BOOL:
        if (h->settermprop_bool) h->settermprop_bool(prop, val->boolean);
        break;
    case VTERM_VALUETYPE_INT:
        if (h->settermprop_int) h->settermprop_int(prop, val->number);
        break;
    case VTERM_VALUETYPE_STRING:
        if (h->settermprop_str)
            h->settermprop_str(prop, val->string.str, val->string.len,
                               val->string.final);
        break;
    default:
        break;
    }
    return 1;
}

static int tramp_bell(void *user) {
    HatCallbacks *h = user;
    if (h->bell) h->bell();
    return 1;
}

static int tramp_sb_pushline(int cols, const VTermScreenCell *cells, void *user) {
    HatCallbacks *h = user;
    if (h->sb_pushline) h->sb_pushline(cols, cells);
    return 1;
}

static int tramp_sb_popline(int cols, VTermScreenCell *cells, void *user) {
    HatCallbacks *h = user;
    if (h->sb_popline) return h->sb_popline(cols, cells);
    return 0;
}

static void tramp_output(const char *bytes, size_t len, void *user) {
    HatCallbacks *h = user;
    if (h->output) h->output(bytes, len);
}

static const VTermScreenCallbacks screen_callbacks = {
    .damage = tramp_damage,
    .moverect = NULL, /* scrolls surface as damage */
    .movecursor = tramp_movecursor,
    .settermprop = tramp_settermprop,
    .bell = tramp_bell,
    .resize = NULL,
    .sb_pushline = tramp_sb_pushline,
    .sb_popline = tramp_sb_popline,
    .sb_clear = NULL,
};

void hat_setup(VTerm *vt, HatCallbacks *cbs) {
    VTermScreen *screen = vterm_obtain_screen(vt);
    vterm_screen_set_callbacks(screen, &screen_callbacks, cbs);
    vterm_screen_enable_reflow(screen, true);
    vterm_output_set_callback(vt, tramp_output, cbs);
}

pid_t hat_spawn_pty(int master_fd, int slave_fd, const char *cwd,
                    const char *file, char *const argv[], char *const envp[]) {
    pid_t pid = fork();
    if (pid != 0)
        return pid;  /* parent (pid > 0) or fork failure (pid < 0) */

    /* child: no Haskell runtime here — plain syscalls until execvpe */
    setsid();
    ioctl(slave_fd, TIOCSCTTY, 0);
    dup2(slave_fd, 0);
    dup2(slave_fd, 1);
    dup2(slave_fd, 2);
    /* Hand the child a clean fd table: only the new stdio survives. This
     * closes master_fd, slave_fd, and — crucially — hat's listening
     * socket, lock, log, and other panes' ptys, none of which are
     * close-on-exec. A pane process that kept the listening socket open
     * outlived `pkill hat` and made the next start's connect() succeed
     * against a socket nobody accepts, hanging forever. */
    close_range(3, ~0U, 0);
    /* Normalize the line discipline to a sane canonical mode. This is the
     * LAST termios write on the pty, by construction: the parent's only
     * (hidden) write — GHC's setRaw when Hat.Pty.spawn makes the master
     * handle unbuffered — happens strictly before this fork. The external
     * `stty sane` this replaces ran after exec, where it raced that parent
     * write and occasionally lost, leaving the pane non-canonical.
     * Retry past EINTR. No tcflush: it would discard input that reached
     * the pty before exec (keys sent to a freshly split pane). */
    {
        struct termios t;
        if (tcgetattr(0, &t) == 0) {
            t.c_iflag |= BRKINT | ICRNL | IXON | IMAXBEL;
            t.c_oflag |= OPOST | ONLCR;
            t.c_cflag |= CREAD | CS8;
            t.c_lflag = ISIG | ICANON | IEXTEN | ECHO | ECHOE | ECHOK
                      | ECHOCTL | ECHOKE;
            while (tcsetattr(0, TCSANOW, &t) == -1 && errno == EINTR)
                ;
        }
    }
    if (cwd != NULL && cwd[0] != '\0') {
        if (chdir(cwd) != 0) {
            /* lenient: exec in the inherited directory if chdir fails */
        }
    }
    execvpe(file, argv, envp);
    _exit(127);
}
