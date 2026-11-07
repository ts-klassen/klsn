#define _GNU_SOURCE

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char **argv)
{
    const int signals[] = {SIGHUP, SIGINT, SIGTERM, SIGPIPE, SIGCHLD};
    struct sigaction action = {0};
    sigset_t mask;
    int descriptors[2] = {-1, -1};
    int saved[2] = {-1, -1};
    int inherited[6] = {-1, -1, -1, -1, -1, -1};

    if (argc < 2) {
        fputs("klsn_bwrap_launcher: missing executable\n", stderr);
        return 125;
    }

    if (sigemptyset(&action.sa_mask) < 0) {
        goto failure;
    }
    if (argc > 3 && strcmp(argv[1], "--ignore-signals") == 0) {
        char *end;
        errno = 0;
        unsigned long long ignored = strtoull(argv[2], &end, 16);
        if (errno != 0 || end == argv[2] || *end != '\0' || argv[2][0] == '-') {
            errno = EINVAL;
            goto failure;
        }
        /* Use the VM's current settings, including signals it no longer
         * ignores. erl_child_setup may have inherited an older snapshot.
         */
        for (int number = 1; number < NSIG; number++, ignored >>= 1) {
            if (number == SIGKILL || number == SIGSTOP) {
                continue;
            }
            action.sa_handler = (ignored & 1) ? SIG_IGN : SIG_DFL;
            /* libc reserves some signal numbers and rejects them. */
            if (sigaction(number, &action, NULL) < 0 && errno != EINVAL) {
                goto failure;
            }
        }
        argc -= 2;
        argv += 2;
    }

    /* Shells cannot reset dispositions inherited as SIG_IGN. GNU and uutils
     * env also differ in option ordering and whether they unblock signals.
     */
    action.sa_handler = SIG_DFL;
    for (size_t i = 0; i < sizeof(signals) / sizeof(signals[0]); i++) {
        if (sigaction(signals[i], &action, NULL) < 0) {
            goto failure;
        }
    }

    if (sigemptyset(&mask) < 0 || sigprocmask(SIG_SETMASK, &mask, NULL) < 0) {
        goto failure;
    }

    if (argc == 3 && strcmp(argv[1], "--pipe-capacity") == 0) {
        descriptors[0] = open(argv[2], O_RDONLY | O_NONBLOCK | O_CLOEXEC);
        if (descriptors[0] < 0) {
            goto failure;
        }
        int capacity = fcntl(descriptors[0], F_GETPIPE_SZ);
        if (capacity < 0) {
            goto failure;
        }
        close(descriptors[0]);
        printf("%d\n", capacity);
        return 0;
    }

    char **command = argv + 1;
    if (argc > 2 && strcmp(argv[1], "--pipes") == 0) {
        /* Bash can block in wait for an already-reaped process substitution
         * while the control watcher remains alive. Create the three data
         * pipes directly instead of using short-lived shell children.
         */
        const int endpoints[][2] = {{3, 8}, {4, 9}, {6, 13}};
        for (size_t i = 0; i < sizeof(endpoints) / sizeof(endpoints[0]); i++) {
            if (pipe2(descriptors, O_CLOEXEC) < 0) {
                goto failure;
            }
            /* Preserve both ends before assigning the fixed descriptors:
             * stdout (3, 8), stderr (4, 9), and stdin (6, 13).
             */
            saved[0] = fcntl(descriptors[0], F_DUPFD_CLOEXEC, 14);
            if (saved[0] < 0) {
                goto failure;
            }
            saved[1] = fcntl(descriptors[1], F_DUPFD_CLOEXEC, 14);
            if (saved[1] < 0) {
                goto failure;
            }
            close(descriptors[0]);
            close(descriptors[1]);
            descriptors[0] = descriptors[1] = -1;
            inherited[2 * i] = dup2(saved[0], endpoints[i][0]);
            if (inherited[2 * i] < 0) {
                goto failure;
            }
            inherited[2 * i + 1] = dup2(saved[1], endpoints[i][1]);
            if (inherited[2 * i + 1] < 0) {
                goto failure;
            }
            close(saved[0]);
            close(saved[1]);
            saved[0] = saved[1] = -1;
        }
        command++;
    }
    execv(command[0], command);

failure: {
    int reason = errno;
    for (int i = 0; i < 2; i++) {
        if (descriptors[i] >= 0) close(descriptors[i]);
        if (saved[i] >= 0) close(saved[i]);
    }
    for (int i = 0; i < 6; i++) {
        if (inherited[i] >= 0) close(inherited[i]);
    }
    fprintf(stderr, "klsn_bwrap_launcher: %s\n", strerror(reason));
    return 125;
}
}
