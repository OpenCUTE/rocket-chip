// See LICENSE.Berkeley for license details.

#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>

#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstdlib>

#include "remote_bitbang.h"

/////////// remote_bitbang_t

remote_bitbang_t::remote_bitbang_t(uint16_t port) :
  socket_fd(0),
  client_fd(0),
  recv_start(0),
  recv_end(0),
  err(0),
  execute_count(0),
  timing_log(NULL)
{
  socket_fd = socket(AF_INET, SOCK_STREAM, 0);
  if (socket_fd == -1) {
    fprintf(stderr, "remote_bitbang failed to make socket: %s (%d)\n",
            strerror(errno), errno);
    abort();
  }

  fcntl(socket_fd, F_SETFL, O_NONBLOCK);
  int reuseaddr = 1;
  if (setsockopt(socket_fd, SOL_SOCKET, SO_REUSEADDR, &reuseaddr,
                 sizeof(int)) == -1) {
    fprintf(stderr, "remote_bitbang failed setsockopt: %s (%d)\n",
            strerror(errno), errno);
    abort();
  }

  // Check for environment variable to override port
  uint16_t actual_port = port;
  if (port == 0) {
    const char* port_env = getenv("REMOTE_BITBANG_PORT");
    if (port_env != NULL) {
      int env_port = atoi(port_env);
      if (env_port > 0 && env_port <= 65535) {
        actual_port = (uint16_t)env_port;
        fprintf(stderr, "Using port from REMOTE_BITBANG_PORT: %d\n", actual_port);
      } else {
        fprintf(stderr, "Invalid REMOTE_BITBANG_PORT value '%s', using auto-assigned port\n", port_env);
      }
    }
  }

  struct sockaddr_in addr;
  memset(&addr, 0, sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = INADDR_ANY;
  addr.sin_port = htons(actual_port);

  if (::bind(socket_fd, (struct sockaddr *) &addr, sizeof(addr)) == -1) {
    fprintf(stderr, "remote_bitbang failed to bind socket: %s (%d)\n",
            strerror(errno), errno);
    abort();
  }

  if (listen(socket_fd, 1) == -1) {
    fprintf(stderr, "remote_bitbang failed to listen on socket: %s (%d)\n",
            strerror(errno), errno);
    abort();
  }

  socklen_t addrlen = sizeof(addr);
  if (getsockname(socket_fd, (struct sockaddr *) &addr, &addrlen) == -1) {
    fprintf(stderr, "remote_bitbang getsockname failed: %s (%d)\n",
            strerror(errno), errno);
    abort();
  }

  tck = 1;
  tms = 1;
  tdi = 1;
  trstn = 1;
  quit = 0;
  execute_count = 0;

  // Check for environment variable to override log file path
  const char* log_path = getenv("REMOTE_BITBANG_LOG");
  if (log_path == NULL) {
    log_path = "remote_bitbang_timing.log";
  }

  // Open timing log file
  timing_log = fopen(log_path, "w");
  if (timing_log) {
    fprintf(timing_log, "remote_bitbang execution timing log\n");
    fprintf(timing_log, "Format: execute_count,command,read_ns,process_ns,total_ns\n");
    fprintf(timing_log, "Log file: %s\n", log_path);
    fflush(timing_log);
    fprintf(stderr, "Remote bitbang timing log: %s\n", log_path);
  } else {
    fprintf(stderr, "Warning: Failed to open timing log file '%s'\n", log_path);
  }

  fprintf(stderr, "This emulator compiled with JTAG Remote Bitbang client. To enable, use +jtag_rbb_enable=1.\n");
  fprintf(stderr, "Listening on port %d\n",
         ntohs(addr.sin_port));
}

remote_bitbang_t::~remote_bitbang_t()
{
  // Close timing log file
  if (timing_log) {
    fprintf(timing_log, "\n=== Log closed ===\n");
    fprintf(timing_log, "Total commands executed: %lu\n", execute_count);
    fclose(timing_log);
    timing_log = NULL;
  }

  // Close socket
  if (socket_fd > 0) {
    close(socket_fd);
  }
  if (client_fd > 0) {
    close(client_fd);
  }
}

void remote_bitbang_t::accept()
{

  fprintf(stderr,"Attempting to accept client socket\n");
  int again = 1;
  while (again != 0) {
    client_fd = ::accept(socket_fd, NULL, NULL);
    if (client_fd == -1) {
      if (errno == EAGAIN) {
        // No client waiting to connect right now.
      } else {
        fprintf(stderr, "failed to accept on socket: %s (%d)\n", strerror(errno),
                errno);
        again = 0;
        abort();
      }
    } else {
      fcntl(client_fd, F_SETFL, O_NONBLOCK);
      fprintf(stderr, "Accepted successfully.");
      again = 0;
    }
  }
}

void remote_bitbang_t::tick(
                            unsigned char * jtag_tck,
                            unsigned char * jtag_tms,
                            unsigned char * jtag_tdi,
                            unsigned char * jtag_trstn,
                            unsigned char jtag_tdo
                            )
{
  if (client_fd > 0) {
    tdo = jtag_tdo;
    execute_command();
  } else {
    this->accept();
  }

  * jtag_tck = tck;
  * jtag_tms = tms;
  * jtag_tdi = tdi;
  * jtag_trstn = trstn;

}

void remote_bitbang_t::reset(){
  //trstn = 0;
}

void remote_bitbang_t::set_pins(char _tck, char _tms, char _tdi){
  tck = _tck;
  tms = _tms;
  tdi = _tdi;
}

void remote_bitbang_t::execute_command()
{
  struct timespec start_read, end_read, end_process;
  clock_gettime(CLOCK_MONOTONIC, &start_read);

  char command;
  int again = 1;
  while (again) {
    ssize_t num_read = read(client_fd, &command, sizeof(command));
    if (num_read == -1) {
      if (errno == EAGAIN) {
        // We'll try again the next call.
        //fprintf(stderr, "Received no command. Will try again on the next call\n");
      } else {
        fprintf(stderr, "remote_bitbang failed to read on socket: %s (%d)\n",
                strerror(errno), errno);
        again = 0;
        abort();
      }
    } else if (num_read == 0) {
      fprintf(stderr, "No Command Received.\n");
      again = 1;
    } else {
      again = 0;
    }
  }

  clock_gettime(CLOCK_MONOTONIC, &end_read);

  //fprintf(stderr, "Received a command %c\n", command);

  int dosend = 0;

  char tosend = '?';

  switch (command) {
  case 'B': /* fprintf(stderr, "*BLINK*\n"); */ break;
  case 'b': /* fprintf(stderr, "_______\n"); */ break;
  case 'r': reset(); break; // This is wrong. 'r' has other bits that indicated TRST and SRST.
  case '0': set_pins(0, 0, 0); break;
  case '1': set_pins(0, 0, 1); break;
  case '2': set_pins(0, 1, 0); break;
  case '3': set_pins(0, 1, 1); break;
  case '4': set_pins(1, 0, 0); break;
  case '5': set_pins(1, 0, 1); break;
  case '6': set_pins(1, 1, 0); break;
  case '7': set_pins(1, 1, 1); break;
  case 'R': dosend = 1; tosend = tdo ? '1' : '0'; break;
  case 'Q': quit = 1; break;
  default:
    fprintf(stderr, "remote_bitbang got unsupported command '%c'\n",
            command);
  }

  if (dosend){
    while (1) {
      ssize_t bytes = write(client_fd, &tosend, sizeof(tosend));
      if (bytes == -1) {
        fprintf(stderr, "failed to write to socket: %s (%d)\n", strerror(errno), errno);
        abort();
      }
      if (bytes > 0) {
        break;
      }
    }
  }

  if (quit) {
    // The remote disconnected.
    fprintf(stderr, "Remote end disconnected\n");
    close(client_fd);
    client_fd = 0;
  }

  clock_gettime(CLOCK_MONOTONIC, &end_process);

  long read_ns = (end_read.tv_sec - start_read.tv_sec) * 1000000000L +
                 (end_read.tv_nsec - start_read.tv_nsec);
  long process_ns = (end_process.tv_sec - end_read.tv_sec) * 1000000000L +
                    (end_process.tv_nsec - end_read.tv_nsec);
  long total_ns = (end_process.tv_sec - start_read.tv_sec) * 1000000000L +
                  (end_process.tv_nsec - start_read.tv_nsec);

  if (timing_log) {
    execute_count++;
    fprintf(timing_log, "%lu,%c,%ld,%ld,%ld\n", execute_count, command, read_ns, process_ns, total_ns);
    fflush(timing_log);
  }
}
