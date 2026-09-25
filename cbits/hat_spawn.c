#define _GNU_SOURCE  /* execvpe, close_range */
#include "hat_spawn.h"

#include <errno.h>
#include <termios.h>
#include <unistd.h>
#include <sys/ioctl.h>

pid_t hat_spawn_pty(int master_fd, int slave_fd, const char *cwd,
                    const char *file, char *const argv[], char *const envp[]) {
    /* Normalize the line discipline to a sane canonical mode, here in the
     * parent before fork: the caller may write to the master as soon as we
     * return, and bytes that arrive while the pty is still raw (GHC's setRaw
     * when Hat.Pty.spawn makes the master handle unbuffered) are merged into
     * one read when ICANON is later switched on — so typed-ahead lines all
     * land in the shell instead of the program it runs next. Retry past
     * EINTR. No tcflush: it would discard input sent to a fresh pane. */
    {
        struct termios t;
        if (tcgetattr(slave_fd, &t) == 0) {
            t.c_iflag |= BRKINT | ICRNL | IXON | IMAXBEL;
            t.c_oflag |= OPOST | ONLCR;
            t.c_cflag |= CREAD | CS8;
            t.c_lflag = ISIG | ICANON | IEXTEN | ECHO | ECHOE | ECHOK
                      | ECHOCTL | ECHOKE;
            while (tcsetattr(slave_fd, TCSANOW, &t) == -1 && errno == EINTR)
                ;
        }
    }

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
    if (cwd != NULL && cwd[0] != '\0') {
        if (chdir(cwd) != 0) {
            /* lenient: exec in the inherited directory if chdir fails */
        }
    }
    execvpe(file, argv, envp);
    _exit(127);
}
