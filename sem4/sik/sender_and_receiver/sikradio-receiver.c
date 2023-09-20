#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>
#include <getopt.h>
#include <stdlib.h>
#include <signal.h>
#include <pthread.h>
#include <time.h>
#include <assert.h>

#include "err.h"
#include "utils.h"

struct LockedData *ld;

void start_new_session(uint64_t session_id, uint64_t first_byte_num) {
    fprintf(stderr, "new session \n");
    ld->session = session_id;
    ld->last_byte_received = first_byte_num;
    ld->first_byte_in_buf = first_byte_num;
    ld->byte_to_read = first_byte_num;
    ld->started_printing = false;
    ld->my_bsize = ld->bsize - ld->bsize % ld->psize;
    ld->received = realloc(ld->received, ld->my_bsize / ld->psize);
    // memset(ld->received, 0, ld->my_bsize / ld->psize);
    for (uint64_t i = 0; i < ld->my_bsize / ld->psize; i++) {
        ld->received[i] = false;
    }
    printf_received(ld);
}

void reader_main() {
    struct sockaddr_in client_address;
    char receive_buf[65535];
    uint64_t session_id = 0;
    uint64_t first_byte_num = 0;
    uint64_t first_byte_debug = 0;
    while(true) {
        first_byte_debug = first_byte_num;
        ld->psize = read_message(ld->socket_fd, &client_address, receive_buf, ld->psize + 16) - 16;
        session_id = be64toh(*((uint64_t*)receive_buf));
        first_byte_num = be64toh(*((uint64_t*)(receive_buf + sizeof(uint64_t))));

        pthread_mutex_lock(&ld->mutex);
        if (session_id > ld->session) {
            start_new_session(session_id, first_byte_num);
        }

        if (session_id == ld->session && first_byte_num >= ld->first_byte_in_buf) {
            ld->last_byte_received = max(ld->last_byte_received, first_byte_num);

            // wypuścić bestię
            if (first_byte_num - ld->first_byte_in_buf >= ld->first_byte_in_buf + 3 * ld->my_bsize / 4) {
                ld->started_printing = true;
                pthread_cond_signal(&ld->start_printing);
            }

            // circular buffer
            if (first_byte_num + ld->psize > ld->my_bsize + ld->first_byte_in_buf) {
                // ld->first_byte_in_buf = max(ld->first_byte_in_buf + ld->my_bsize, first_byte_num - ld->my_bsize + ld->psize);
                if (ld->first_byte_in_buf + ld->my_bsize < first_byte_num - ld->my_bsize + ld->psize) {
                    // many missing packets - save last raceived packet to the end of the buffer
                    memset(ld->received, 0, ld->my_bsize / ld->psize);
                    ld->first_byte_in_buf = first_byte_num + ld->psize - ld->my_bsize;
                } else {
                    fprintf(stderr, "cykl kurwa\n");
                    ld->first_byte_in_buf += ld->my_bsize;
                    assert(first_byte_num >= ld->first_byte_in_buf);
                    assert(first_byte_num - ld->first_byte_in_buf < ld->my_bsize);
                    for (uint64_t i = ld->first_byte_in_buf; i < first_byte_num; i += ld->psize) {
                        ld->received[(i - ld->first_byte_in_buf) / ld->psize] = 0;
                    }
                }
            }
            
            // printing all missing packets
            for (uint64_t i = ld->byte_to_read; i < first_byte_num; i += ld->psize) {
                while (i < ld->first_byte_in_buf) {
                    i += ld->my_bsize;
                }
                assert(i - ld->first_byte_in_buf < ld->my_bsize);
                if (i != first_byte_num && (i >= ld->first_byte_in_buf + ld->my_bsize || !ld->received[(i - ld->first_byte_in_buf) / ld->psize])) {
                    fprintf(stderr, "MISSING: BEFORE %lu EXPECTED %lu\n", first_byte_num / ld->psize, i / ld->psize);
                }
            }

            // not to double buffer
            if (first_byte_num == ld->byte_to_read + ld->bsize) {
                ld->byte_to_read += ld->psize;
            }
        
            // copy music data to buffer
            assert(first_byte_num >= ld->first_byte_in_buf);
            memcpy(ld->data + (first_byte_num - ld->first_byte_in_buf), receive_buf + 16, ld->psize);
            assert(first_byte_num >= ld->first_byte_in_buf && first_byte_num - ld->first_byte_in_buf < ld->my_bsize);
            ld->received[(first_byte_num - ld->first_byte_in_buf) / ld->psize] = true;
            if (ld->last_byte_received >= ld->byte_to_read) {
                pthread_cond_signal(&ld->protect_data);
            }
        }
        pthread_mutex_unlock(&ld->mutex);
    }
}

void* writer_main() {
    uint64_t index_to_read;
    char *buf_to_print = malloc(65536);

    while(true) {
        pthread_mutex_lock(&ld->mutex);
        while (!ld->started_printing) {
        pthread_cond_wait(&ld->start_printing, &ld->mutex);
        }
        while (ld->byte_to_read > ld->last_byte_received) {
            pthread_cond_wait(&ld->protect_data, &ld->mutex);
        }
        if (!ld->running) break;
        if (ld->byte_to_read < ld->first_byte_in_buf) {
            if (ld->byte_to_read + ld->my_bsize > ld->first_byte_in_buf) {
                index_to_read = ld->byte_to_read - ld->first_byte_in_buf + ld->my_bsize;
            }
            else {
                ld->byte_to_read = ld->first_byte_in_buf;
                index_to_read = 0;
            }
        } else {
            index_to_read = ld->byte_to_read - ld->first_byte_in_buf;
        }
        assert(index_to_read % ld->psize == 0);

        // if the packet wasn't received yet, print 0s
        assert(index_to_read < ld->my_bsize);
        if (!ld->received[index_to_read / ld->psize]) {;
            for (uint64_t i = 0; i < ld->psize; i++) {
                buf_to_print[i] = 0;
            }
        }
        else {
            assert(index_to_read < ld->my_bsize);
            memcpy(buf_to_print, ld->data + index_to_read, ld->psize);
            ld->received[index_to_read / ld->psize] = false;
        }

        ld->byte_to_read += ld->psize;

        pthread_mutex_unlock(&ld->mutex);
        fwrite(buf_to_print, 1, ld->psize, stdout);
    }
    return 0;
}

int main(int argc, char *argv[]) {
    uint16_t data_port = 29978;
    size_t bsize = 655368;
    const char* src_addr = NULL;
    int flag;
    while((flag = getopt(argc, argv, "a:P:b:")) != -1) {
        switch(flag) {
            case 'a':
                src_addr = optarg;
                break;
            case 'P':
                data_port = read_port(optarg);
                break;
            case 'b':
                bsize = atoi(optarg);
                break;
            default:
                fatal("Nieznany argument");
        }
    }
    if(src_addr == NULL) {
        fatal("No source address given");
    }
    char *receive_buf = malloc(65536 + 16);

    int socket_fd = bind_socket(data_port);

    size_t psize;
    struct sockaddr_in client_address;
    psize = read_message(socket_fd, &client_address, receive_buf, 65535) - 16;
    uint64_t session_id;
    memcpy(&session_id, receive_buf, sizeof(uint64_t));

    session_id = be64toh(session_id);

    ld = malloc(sizeof(struct LockedData));
    locked_data_init(ld, bsize, psize, socket_fd, session_id);

    memcpy(ld->data, receive_buf + 2 * sizeof(uint64_t), psize);
    ld->received[0] = true;

    pthread_t writer_thread;
    pthread_create(&writer_thread, NULL, writer_main, NULL);
    reader_main();

    return 0;
}