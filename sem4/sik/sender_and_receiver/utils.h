#ifndef UTILS_H
#define UTILS_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdatomic.h>

#include <endian.h>
#include <byteswap.h>

struct packet {
  uint64_t session_id;
  uint64_t first_byte_num;
  char *data;
};

struct LockedData {
  pthread_mutex_t mutex;
  pthread_cond_t protect_data;
  pthread_cond_t start_printing;
  uint64_t last_byte_received;
  uint64_t socket_fd;
  uint64_t first_byte_in_buf;
  uint64_t byte_to_read;
  char *data;
  bool *received;
  uint64_t session;
  uint64_t psize;
  uint64_t bsize;
  bool started_printing;
  atomic_bool running;
  uint64_t my_bsize;
  uint64_t byte_zero;
};


uint64_t max(uint64_t a, uint64_t b) {
  if (a > b) {
    return a;
  }
  return b;
}

uint64_t min(uint64_t a, uint64_t b) {
  if (a < b) {
    return a;
  }
  return b;
}

void locked_data_init(struct LockedData* ld, uint64_t bsize, uint64_t psize, uint64_t socket_fd, uint64_t session) {
  pthread_mutex_init(&ld->mutex, NULL);
  pthread_cond_init(&ld->protect_data, NULL);
  pthread_cond_init(&ld->start_printing, NULL);
  ld->first_byte_in_buf = 0;
  ld->byte_to_read = 0;
  ld->last_byte_received = 0;
  ld->psize = psize;
  // ld->bsize = bsize - bsize % psize;
  ld->bsize = bsize;
  ld->my_bsize = bsize - bsize % psize; // real bsize for current session
  ld->data = malloc(ld->bsize);
  ld->received = calloc(ld->my_bsize / psize, sizeof(bool));
  ld->socket_fd = socket_fd;
  ld->session = session;
  ld->started_printing = false;
  ld->running = true;
}

void locked_data_destroy(struct LockedData* ld) {
  pthread_cond_destroy(&ld->protect_data);
  pthread_cond_destroy(&ld->start_printing);
  pthread_mutex_destroy(&ld->mutex);
  free(ld->data);
  free(ld->received);
  free(ld);
}

uint16_t read_port(char *string) {
    errno = 0;
    unsigned long port = strtoul(string, NULL, 10);
    PRINT_ERRNO();
    if (port > UINT16_MAX) {
        fatal("%u is not a valid port number", port);
    }

    return (uint16_t) port;
}

int bind_socket(uint16_t port) {
    int socket_fd = socket(AF_INET, SOCK_DGRAM, 0); // creating IPv4 UDP socket
    ENSURE(socket_fd >= 0);
    // after socket() call; we should close(sock) on any execution path;

    struct sockaddr_in server_address;
    server_address.sin_family = AF_INET; // IPv4
    server_address.sin_addr.s_addr = htonl(INADDR_ANY); // listening on all interfaces
    server_address.sin_port = htons(port);

    // bind the socket to a concrete address
    CHECK_ERRNO(bind(socket_fd, (struct sockaddr *) &server_address,
                        (socklen_t) sizeof(server_address)));

    return socket_fd;
}

size_t read_message(int socket_fd, struct sockaddr_in *client_address, void *buffer, size_t max_length) {
    socklen_t address_length = (socklen_t) sizeof(*client_address);
    int flags = 0; // we do not request anything special
    errno = 0;
    ssize_t len = recvfrom(socket_fd, buffer, max_length, flags,
                           (struct sockaddr *) client_address, &address_length);
    if (len < 0) {
        PRINT_ERRNO();
    }
    return (size_t) len;
}

uint64_t htonll(uint64_t v) {
    union { uint32_t lv[2]; uint64_t llv; } u;
    u.lv[0] = htonl(v >> 32);
    u.lv[1] = htonl(v & 0xFFFFFFFFULL);
    return u.llv;
}

uint64_t ntohll(uint64_t v) {
    union { uint32_t lv[2]; uint64_t llv; } u;
    u.llv = v;
    return ((uint64_t)ntohl(u.lv[0]) << 32) | (uint64_t)ntohl(u.lv[1]);
}

void printf_received(struct LockedData *ld) {
  fprintf(stderr, "received: ");
  for (uint64_t i = 0; i < ld->bsize / ld->psize; i++) {
    fprintf(stderr, "%d", ld->received[i]);
  }
  fprintf(stderr, "\n");
}

#endif // UTILS_H