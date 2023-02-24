#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <string.h>
#include "pipeline-utils.h"

#include "err.h"

int main(int argc, char *argv[]) {
    if (argc < 2) 
        fatal("Usage: %s <read_fd>\n", argv[0]);

    int pipe_dsc[2];
    ASSERT_SYS_OK(pipe(pipe_dsc));
    const char* program_name = argv[1];


    pid_t pid;
    ASSERT_SYS_OK(pid = fork());
    int i;
    for (i = 1; pid == 0 && i < argc - 1; i++) {
        ASSERT_SYS_OK(close(pipe_dsc[1]));
        ASSERT_SYS_OK(dup2(pipe_dsc[0], 0));
        ASSERT_SYS_OK(close(pipe_dsc[0]));

        ASSERT_SYS_OK(pipe(pipe_dsc));

        program_name = argv[i + 1];
        if (i < argc - 2)
            ASSERT_SYS_OK(pid = fork());
    }

    ASSERT_SYS_OK(close(pipe_dsc[0]));

    if (i < argc - 1) {
        ASSERT_SYS_OK(dup2(pipe_dsc[1], 1));
    }
    ASSERT_SYS_OK(close(pipe_dsc[1]));
    //print_open_descriptors();
    ASSERT_SYS_OK(execlp(program_name, program_name, NULL));

    //print_open_descriptors();
    return 0;
}