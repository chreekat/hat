#ifndef HAT_SPAWN_H
#define HAT_SPAWN_H

#include <sys/types.h>

/* Spawn a child that becomes the session leader of the given pty slave,
 * redirects stdio to it, normalizes the line discipline to canonical mode
 * (in the child's own foreground context, so it sticks), chdir()s (if cwd
 * is non-NULL), and execs. Runs entirely in C, so no Haskell/RTS code
 * executes in the child between fork and exec (avoiding the threaded-RTS
 * fork hazard under load). Returns the child pid, or -1 on failure.
 *
 * Pty plumbing, unrelated to the terminal-emulator shim, so it lives in its
 * own translation unit. */
pid_t hat_spawn_pty(int master_fd, int slave_fd, const char *cwd,
                    const char *file, char *const argv[], char *const envp[]);

#endif
