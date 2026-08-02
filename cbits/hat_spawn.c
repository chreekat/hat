#define _GNU_SOURCE  /* execvpe, close_range */
#include "hat_spawn.h"

#include <errno.h>
#include <termios.h>
#include <unistd.h>
#include <sys/ioctl.h>

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
