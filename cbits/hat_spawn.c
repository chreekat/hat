#define _GNU_SOURCE  /* execvpe, close_range */
#include "hat_spawn.h"

#include <errno.h>
#include <sched.h>
#include <signal.h>
#include <termios.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/mman.h>

/* What the child needs, read from the parent's memory it borrows. */
struct spawn_args {
    int slave_fd;
    const char *cwd;
    const char *file;
    char *const *argv;
    char *const *envp;
    sigset_t exec_mask;  /* the mask the program starts with */
};

/* The child, on a stack of its own: no Haskell runtime here, plain
 * syscalls until execvpe, and no writes to the memory it shares with the
 * parent beyond that stack. */
static int spawn_child(void *p) {
    const struct spawn_args *a = p;
    /* Every signal is blocked on entry. Drop the parent's handlers before
     * unblocking, so none can run in the borrowed memory. */
    for (int sig = 1; sig < NSIG; sig++) {
        struct sigaction sa;
        if (sigaction(sig, NULL, &sa) == 0 && sa.sa_handler != SIG_IGN
                && sa.sa_handler != SIG_DFL) {
            sa.sa_handler = SIG_DFL;
            sa.sa_flags = 0;
            sigaction(sig, &sa, NULL);
        }
    }
    sigprocmask(SIG_SETMASK, &a->exec_mask, NULL);
    setsid();
    ioctl(a->slave_fd, TIOCSCTTY, 0);
    dup2(a->slave_fd, 0);
    dup2(a->slave_fd, 1);
    dup2(a->slave_fd, 2);
    /* Hand the child a clean fd table: only the new stdio survives. This
     * closes master_fd, slave_fd, and — crucially — hat's listening
     * socket, lock, log, and other panes' ptys, none of which are
     * close-on-exec. A pane process that kept the listening socket open
     * outlived `pkill hat` and made the next start's connect() succeed
     * against a socket nobody accepts, hanging forever. */
    close_range(3, ~0U, 0);
    if (a->cwd != NULL && a->cwd[0] != '\0') {
        if (chdir(a->cwd) != 0) {
            /* lenient: exec in the inherited directory if chdir fails */
        }
    }
    execvpe(a->file, a->argv, a->envp);
    _exit(127);
}

/* Enough for execvpe's on-stack path and script-fallback buffers. */
#define SPAWN_STACK_SIZE (256 * 1024)

pid_t hat_spawn_pty(int master_fd, int slave_fd, const char *cwd,
                    const char *file, char *const argv[], char *const envp[]) {
    (void)master_fd;  /* closed in the child by close_range */
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

    /* CLONE_VM | CLONE_VFORK, as glibc's posix_spawn: the child borrows
     * the parent's memory until it execs, so the spawn copies no page
     * tables and leaves the parent no copy-on-write faults, however large
     * its heap. The calling thread waits until the exec; signals stay
     * blocked in it meanwhile. */
    void *stack = mmap(NULL, SPAWN_STACK_SIZE, PROT_READ | PROT_WRITE,
                       MAP_PRIVATE | MAP_ANONYMOUS | MAP_STACK, -1, 0);
    if (stack == MAP_FAILED)
        return -1;
    struct spawn_args args = {
        .slave_fd = slave_fd, .cwd = cwd, .file = file, .argv = argv,
        .envp = envp,
    };
    sigset_t all;
    sigfillset(&all);
    pthread_sigmask(SIG_SETMASK, &all, &args.exec_mask);
    pid_t pid = clone(spawn_child, (char *)stack + SPAWN_STACK_SIZE,
                      CLONE_VM | CLONE_VFORK | SIGCHLD, &args);
    pthread_sigmask(SIG_SETMASK, &args.exec_mask, NULL);
    munmap(stack, SPAWN_STACK_SIZE);
    return pid;  /* the child's pid, or -1 when the clone failed */
}
