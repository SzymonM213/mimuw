// Patrz man basename, chcemy wersję GNU
#define _GNU_SOURCE

#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <arpa/inet.h>
#include <endian.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <netdb.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <time.h>

#include "err.h"
#include "utils.h"

#define HEADER_SIZE 16

uint16_t DATA_PORT = 29978;
size_t PSIZE = 512;
const char* NAZWA = "Nienazwany Nadajnik";
const char* DEST_ADDR = NULL;

struct sockaddr_in get_send_address(const char *host, uint16_t port) {
    struct addrinfo hints;
    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_INET; // IPv4
    hints.ai_socktype = SOCK_DGRAM;
    hints.ai_protocol = IPPROTO_UDP;

    struct addrinfo *address_result;
    CHECK(getaddrinfo(host, NULL, &hints, &address_result));

    struct sockaddr_in send_address;
    send_address.sin_family = AF_INET; // IPv4
    send_address.sin_addr.s_addr =
            ((struct sockaddr_in *) (address_result->ai_addr))->sin_addr.s_addr; // IP address
    send_address.sin_port = htons(port); // port from the command line

    freeaddrinfo(address_result);

    return send_address;
}

void send_uint64(int socket_fd, const struct sockaddr_in *send_address, uint64_t n) {
    int send_flags = 0;
    socklen_t address_length = (socklen_t) sizeof(*send_address);
    errno = 0;
    ssize_t sent_length = sendto(socket_fd, &n, sizeof(uint64_t), send_flags, (struct sockaddr *) send_address, address_length);
    if (sent_length < 0) {
        PRINT_ERRNO();
    }
    ENSURE(sent_length == (ssize_t) sizeof(uint64_t));
}

void send_message(int socket_fd, const struct sockaddr_in *send_address, const char *data) {
    int send_flags = 0;
    socklen_t address_length = (socklen_t) sizeof(*send_address);
    errno = 0;
    ssize_t sent_length = sendto(socket_fd, data, PSIZE + 16, send_flags, (struct sockaddr *) send_address, address_length);
    if (sent_length < 0) {
        PRINT_ERRNO();
    }
    // ENSURE(sent_length == PSIZE + 16);
}

void send_packet(int socket_fd, const struct sockaddr_in *send_address, struct packet *p) {
  send_uint64(socket_fd, send_address, p->session_id);
  send_uint64(socket_fd, send_address, p->first_byte_num);
  send_message(socket_fd, send_address, p->data);
}

void print_packet(char *p) {
  for (uint64_t i = 16; i < 16 + PSIZE; i++) {
    printf("%c", p[i]);
  }
}

int main(int argc, char* argv[]) {
  size_t session_id = time(NULL);
  printf("session_id: %lu\n", session_id);
  int flag;
  while((flag = getopt(argc, argv, "a:P:p:n:")) != -1) {
    switch(flag) {
      case 'a':
        DEST_ADDR = optarg;
        break;
      case 'P':
        DATA_PORT = read_port(optarg);
        break;
      case 'p':
        PSIZE = atoi(optarg);
        break;
      case 'n':
        NAZWA = optarg;
        break;
      default:
        fatal("Unknown argument");
    }
  }
  if(DEST_ADDR == NULL) {
    fatal("No destination address given");
  }

  struct sockaddr_in send_address = get_send_address(DEST_ADDR, DATA_PORT);

  int socket_fd = socket(PF_INET, SOCK_DGRAM, 0);
  if (socket_fd < 0) {
      PRINT_ERRNO();
  }

  uint64_t first_byte_num = 0;
  char *packet = malloc(PSIZE + 2 * sizeof(uint64_t));

  session_id = htobe64(session_id);
  printf("session_id: %lu\n", session_id);

  uint64_t first_byte_to_send;
  memcpy(packet, &session_id, sizeof(uint64_t));
  while(fread(packet + 2 * sizeof(uint64_t), 1, PSIZE, stdin)) {

    first_byte_to_send = htobe64(first_byte_num);

    memcpy(packet + sizeof(uint64_t), &first_byte_to_send, sizeof(uint64_t));
    printf("session: %lu, first byte: %lu\n", session_id, first_byte_num);
    send_message(socket_fd, &send_address, packet);
    first_byte_num += PSIZE;
  }
  free(packet);
}