#include "utils.h"

size_t max(size_t a, size_t b)
{
    if (a > b)
        return a;
    return b;
}

size_t more(size_t n, int line, maze *m)
{
    if (n == SIZE_MAX)
        error(line, m);
    if (n >= SIZE_MAX / 3 * 2)
        return n + 1;
    else
    {
        if (n == 1)
            n = 2;
        return n / 2 * 3 + 1;
    }
}

//swaps byte halves
void reverse_byte(char *a)
{
    (*a) = (char)(((*a) & 0xF0) >> 4 | ((*a) & 0x0F) << 4);
}

// reverses array of chars to make more significant bits on the end
void reverse_array(char *a, size_t size)
{
    char tmp;
    for (size_t i = 0; i < size / 2; i++)
    {
        tmp = a[i];
        a[i] = a[size - i - 1];
        a[size - i - 1] = tmp;
        reverse_byte(&a[i]);
        reverse_byte(&a[size - i - 1]);
    }
    if (size % 2)
        reverse_byte(&a[size / 2]);
}

void free_maze(maze *m)
{
    free(m->n);
    free(m->x);
    free(m->y);
    free(m->walls);
    free(m);
}

// frees allocated memory and ends program with error
void error(int i, maze *m)
{
    free_maze(m);
    fprintf(stderr, "ERROR %d\n", i);
    exit(EXIT_FAILURE);
}

size_t div_round_up(size_t divident, size_t divisor)
{
    if (divident % divisor)
        return divident / divisor + 1;
    else
        return divident / divisor;
}

// returns 1 iff c is blank char and isn't an endline
int better_isspace(int c)
{
    return (isspace(c) && c != '\n' && c != '\r');
}

// malloc but it's aware of memory limit
void *better_malloc(size_t size, maze *m)
{
    void *a = malloc(size);
    if (a == NULL && size > 0)
        error(0, m);
    return a;
}

// calloc but it's aware of memory limit
void *better_calloc(size_t n, size_t size, maze *m)
{
    void *a = calloc(n, size);
    if (a == NULL && n > 0 && size > 0)
        error(0, m);
    return a;
}

// realloc but it's aware of memory limit
void *better_realloc(void *a, size_t size, maze *m)
{
    void *b = realloc(a, size);
    if (b == NULL && size > 0)
        error(0, m);
    return b;
}

// realloc + calloc
char *recalloc(char *pointer, size_t *previous_size, 
               size_t expected_size, maze *m)
{
    pointer = better_realloc(pointer, (size_t)expected_size * sizeof(char), m);
    if (pointer == NULL)
        error(0, m);
    for (size_t i = (*previous_size); i < expected_size; i++)
        pointer[i] = 0;
    (*previous_size) = expected_size;
    return pointer;
}

// checks if read line was proper
void check_line(int line_number, size_t *line, size_t length, maze *m)
{
    if ((line_number != 1 && length != m->dimensions) || length == 0)
        error(line_number, m);
    if (line_number != 1)
    {
        for (size_t i = 0; i < length; i++)
            if (line[i] > (m->n)[i])
                error(line_number, m);
    }
    else
    {
        size_t n_product = 1;
        for (size_t i = 0; i < length; i++)
        {
            if (n_product > SIZE_MAX / line[i])
                error(line_number, m);
            n_product *= line[i];
        }
    }
}

// function that ends program if eof was read and decides which line wasn't ok
void end_of_file(int line_number, size_t *line, size_t actual_length, maze *m)
{
    if (line_number == 1)
        check_line(line_number, line, actual_length, m);
    else
        check_line(line_number, line, actual_length, m);
    error(line_number + 1, m);
}

size_t biggest_bit(char *bit_array, size_t number_of_bytes)
{
    size_t i = number_of_bytes * 8;
    while (i > 1 && !(bit_array[(i - 1) / 8] & 1 << ((i - 1) % 8)))
        i--;
    return i - 1;
}

size_t vertex_index(size_t *vertex, size_t *n, size_t k)
{
    size_t index = vertex[k - 1] - 1;
    for (size_t i = k - 1; i >= 1; i--)
    {
        index *= n[i - 1];
        index += vertex[i - 1] - 1;
    }
    return index;
}

