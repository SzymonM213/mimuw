#ifndef UTILS_H
#define UTILS_H
#include <assert.h>
#include <inttypes.h>
#include <limits.h>
#include <malloc.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct maze
{
    size_t dimensions;
    size_t *n;
    size_t *x;
    size_t *y;
    char *walls;
    size_t fields;
} maze;

size_t max(size_t a, size_t b);

size_t more(size_t n, int line, maze *m);

void reverse_array(char *a, size_t size);

void reverse_byte(char *a);

void error(int i, maze *m);

size_t div_round_up(size_t divident, size_t divisor);

int better_isspace(int c);

void *better_malloc(size_t size, maze *m);

void *better_calloc(size_t n, size_t size, maze *m);

void *better_realloc(void *a, size_t size, maze *m);

char *recalloc(char *pointer, size_t *previous_size, 
               size_t expected_size, maze *m);

void check_line(int line_number, size_t *line, size_t length, maze *m);

void end_of_file(int line_number, size_t *line, size_t actual_length, maze *m);

size_t biggest_bit(char *bit_array, size_t number_of_bits);

size_t vertex_index(size_t *vertex, size_t *n, size_t k);

void free_maze(maze *m);

#endif 