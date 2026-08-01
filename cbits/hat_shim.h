#ifndef HAT_SHIM_H
#define HAT_SHIM_H

#include <stddef.h>
#include <stdint.h>
#include <sys/types.h>
#include <vterm.h>

/* A VTermScreenCell flattened for the Haskell FFI: no bitfields, no
 * tagged unions. Color kind: 0 = default, 1 = indexed, 2 = rgb. */
typedef struct {
    uint32_t chars[VTERM_MAX_CHARS_PER_CELL];
    int width;
    unsigned flags; /* 1 bold, 2 underline, 4 italic, 8 reverse, 16 strike, 32 blink, 64 dim */
    int fg_kind, fg_idx, fg_r, fg_g, fg_b;
    int bg_kind, bg_idx, bg_r, bg_g, bg_b;
} HatCell;

/* Haskell-friendly callback table: scalar arguments only. Any pointer may
 * be NULL. Registered on the vterm instance by hat_setup. */
typedef struct {
    void (*damage)(int start_row, int end_row, int start_col, int end_col);
    void (*movecursor)(int row, int col, int visible);
    void (*settermprop_bool)(int prop, int val);
    void (*settermprop_int)(int prop, int val);
    void (*settermprop_str)(int prop, const char *str, size_t len, int final);
    void (*bell)(void);
    void (*sb_pushline)(int cols, const VTermScreenCell *cells);
    int (*sb_popline)(int cols, VTermScreenCell *cells);
    void (*output)(const char *bytes, size_t len);
} HatCallbacks;

void hat_setup(VTerm *vt, HatCallbacks *cbs);
int hat_get_cell(VTermScreen *screen, int row, int col, HatCell *out);
/* Flatten the state's current pen (the SGR the next glyph would take) into a
 * HatCell, so its style is read back exactly like a cell's. */
int hat_get_pen(VTerm *vt, HatCell *out);
void hat_flatten_cell_at(const VTermScreenCell *cells, int i, HatCell *out);
void hat_unflatten_cell_at(VTermScreenCell *cells, int i, const HatCell *in);

/* fork() a child that becomes the session leader of the given pty slave,
 * redirects stdio to it, normalizes the line discipline to canonical mode
 * (in the child's own foreground context, so it sticks), chdir()s (if cwd
 * is non-NULL), and execs. Runs entirely in C, so no Haskell/RTS code
 * executes in the child between fork and exec (avoiding the threaded-RTS
 * fork hazard under load). Returns the child pid, or -1 on fork failure. */
pid_t hat_spawn_pty(int master_fd, int slave_fd, const char *cwd,
                    const char *file, char *const argv[], char *const envp[]);

#endif
