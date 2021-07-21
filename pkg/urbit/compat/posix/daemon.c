#include "all.h"
#include "vere/vere.h"
#include <sys/wait.h>

/*
  This is set to the the write-end of a pipe when Urbit is started in
  daemon mode. It's meant to be used as a signal to the parent process
  that the child process has finished booting.
*/
static c3_i _child_process_booted_signal_fd = -1;

/*
  This should be called whenever the ship has been booted enough to
  handle commands from automation code. Specifically, once the Eyre's
  `chis` interface is up and running.

  In daemon mode, this signals to the parent process that it can
  exit. Otherwise, it does nothing.

  Once we've sent a signal with `write`, we close the file descriptor
  and overwrite the global to make it impossible to accidentally do
  this twice.
*/
static void _on_boot_completed_cb() {
  c3_c buf[2] = {0,0};

  if ( -1 == _child_process_booted_signal_fd ) {
    return;
  }

  if ( 0 == write(_child_process_booted_signal_fd, buf, 1) ) {
    c3_assert(!"_on_boot_completed_cb: Can't write to parent FD");
  }

  close(_child_process_booted_signal_fd);
  _child_process_booted_signal_fd = -1;
}

/* u3_daemon_init(): platform-specific daemon mode initialization.

  We use a pipe to communicate between the child and the parent. The
  parent waits for the child to write something to the pipe and
  then exits. If the pipe is closed with nothing written to it, get
  the exit status from the child process and also exit with that status.

  We want the child to write to the pipe once it's booted, so we put
  `_on_boot_completed_cb` into `u3_Host.bot_f`, which is NULL in
  non-daemon mode. That gets called once the `chis` service is
  available.

  In both processes, we are good fork() citizens, and close all unused
  file descriptors. Closing `pipefd[1]` in the parent process is
  especially important, since the pipe needs to be closed if the child
  process dies. When the pipe is closed, the read fails, and that's
  how we know that something went wrong.

  There are some edge cases around `WEXITSTATUS` that are not handled
  here, but I don't think it matters.
*/
void
u3_daemon_init()
{
  c3_i pipefd[2];

  if ( 0 != pipe(pipefd) ) {
    c3_assert(!"Failed to create pipe");
  }

  pid_t childpid = fork();

  if ( 0 == childpid ) {
    close(pipefd[0]);
    _child_process_booted_signal_fd = pipefd[1];
    u3_Host.bot_f = _on_boot_completed_cb;
    return;
  }

  close(pipefd[1]);
  close(0);
  close(1);
  close(2);

  c3_c buf[2] = {0,0};
  if ( 1 == read(pipefd[0], buf, 1) ) {
    exit(0);
  }

  c3_i status;
  wait(&status);
  exit(WEXITSTATUS(status));
}
