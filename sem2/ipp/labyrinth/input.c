#include "input.h"
#include "bfs.h"
#include "utils.h"



static int skip_space_dont_skip_digit(int c)
{
    if (c == '\n' || c == '\r' || (c >= '0' && c <= '9'))
        return c;
    while (better_isspace(c = getchar()));
    return c;
}

// function that reads number from input which begins from *c
static size_t read_number(int *c, size_t k, int line_number, 
                          size_t *a, maze *m)
{
    if (*c < '0' || *c > '9')
    {
        if ((*c) != -1)
            error(line_number, m);
        end_of_file(line_number, a, k, m);
    }
    size_t result = (size_t)(*c) - '0';
    while (((*c) != -1) && !(better_isspace((*c) = getchar())) 
    &&    ((*c) != '\n') && ((*c) != '\r'))
    {
        if (*c < '0' || *c > '9')
        {
            if ((*c) != -1)
                error(line_number, m);
        }
        if (*c != -1)
        {
            if (result > (SIZE_MAX - ((size_t)(*c) - '0')) / 10)
                error(line_number, m);
            result = 10 * result + ((size_t)(*c) - '0');
        }
    }
    return result;
}

// main function to read lines 1-3
static void read_line(size_t *k, int line_number, maze *m, size_t **a)
{
    int c = -2;
    (*k) = 0;
    *a = NULL;
    size_t i;
    // every cycle of this loop gives us new number from input
    for (i = 0; (c = skip_space_dont_skip_digit(c)) != '\n' && c != '\r'; i++)
    {
        if (c < '0' || c > '9')
        {
            if (c != -1 || i == 0)
                error(line_number, m);
            end_of_file(line_number, *a, i, m);
        }
        if (i == (*k))
        {
            *k = more(*k, line_number, m);
            *a = better_realloc(*a, (size_t)(*k) * sizeof(**a), m);
        }
        (*a)[i] = read_number(&c, i, line_number, *a, m);
        if ((*a)[i] == 0)
            error(line_number, m);
    }
    if (c == 13)
        c = getchar();
    *a = better_realloc(*a, (size_t)i * sizeof(**a), m);
    (*k) = i;
}

// read_number but accepts EOF on the end
static size_t read_number_in_last_line(int *c, maze *m)
{
    if (*c < '0' || *c > '9')
    {
        error(LAST_LINE, m);
    }
    size_t result = (size_t)(*c) - '0';
    while (!better_isspace((*c) = getchar()) && ((*c) != '\n') 
    &&    ((*c) != '\r') && (*c) != EOF)
    {
        if (*c < '0' || *c > '9')
        {
            error(LAST_LINE, m);
        }
        if (10 * result > SIZE_MAX - ((size_t)(*c) - '0'))
            error(LAST_LINE, m);
        result = 10 * result + ((size_t)(*c) - '0');
    }
    return result;
}

// reads the hex number and converts it into binary but it's not exactly what 
// we need (it's inverted and might have random zeros on the end)
static void read_hex_return_bin(size_t *bits, size_t *size_of_input, maze *m)
{
    int c = getchar();
    if (!isxdigit(c))
        error(LAST_LINE, m);
    while (c == '0')
        c = getchar();
    size_t size = div_round_up(m->fields, 8);
    m->walls = better_calloc(size, sizeof(char), m);
    size_t i;
    for (i = 0;; i++)
    {
        if (c >= '0' && c <= '9')
            c -= '0';
        else if (c >= 'a' && c <= 'f')
            c = c - 'a' + 10;
        else if (c >= 'A' && c <= 'F')
            c = c - 'A' + 10;
        else if (c != '\n' && c != '\r' && c != EOF && !(better_isspace(c)))
            error(LAST_LINE, m);
        else
            break;
        if (i * 4 > m->fields)
            error(LAST_LINE, m);
        if (i % 2)
            (m->walls)[i / 2] |= (char)(c << 4);
        else
            (m->walls)[i / 2] |= (char)c;
        c = getchar();
    }
    *bits = ((i - 1) / 2 + 1) * 8;
    *size_of_input = i;
    if (better_isspace(c))
        c = skip_space_dont_skip_digit(c);
    if (c == '\r')
        c = getchar();
    if (c == '\n' && (c = getchar()) != EOF)
        error(LAST_LINE + 1, m);
    // the 5th line didn't begin but something unexpected left in the 4th line
    if (c != EOF)
        error(LAST_LINE, m);
}

// fixes array walls to make it readable the way we need it
static void fix_walls(size_t bits, size_t size_of_input, maze *m)
{
    reverse_array(m->walls, bits / 8);
    bits=div_round_up(m->fields, 8)*8;
    if (size_of_input % 2)
    {
        char *proper_walls = calloc(bits / 8, sizeof(char));
        for (size_t i = 0; i < bits - 4; i++)
        {
            proper_walls[i / 8] |= (char)((((m->walls)[(i + 4) / 8] & 1 <<
            ((i + 4) % 8)) >> ((i + 4) % 8)) << (i % 8));
        }
        memcpy(m->walls, proper_walls, bits / 8);
        free(proper_walls);
    }
}

static size_t read_number_between_spaces(int *c, maze *m)
{
    *c = skip_space_dont_skip_digit(*c);
    return read_number_in_last_line(c, m);
}

static void make_walls(size_t *letters, maze *m)
{
    m->walls = better_calloc(div_round_up(m->fields, 8), sizeof(char), m);
    size_t tmp;
    for (size_t i = 1; i <= letters[3]; i++)
    {
        letters[4] = (letters[0] * letters[4] + letters[1]) % letters[2];
        tmp = letters[4] % (m->fields);
        while (tmp < (m->fields))
        {
            (m->walls)[(tmp) / 8] |= (char)(1 << (tmp) % 8);
            tmp += UINT32_MAX;
            tmp++;
        }
    }
}

// main function to read last line
static void read_last_line(size_t *bits, maze *m)
{
    m->walls = NULL;
    size_t size_of_input;
    int c = -2;
    c = skip_space_dont_skip_digit(c);
    if (c == '0')
    {
        if ((c = getchar()) != 'x')
            error(LAST_LINE, m);
        read_hex_return_bin(bits, &size_of_input, m);
        fix_walls(*bits, size_of_input, m);
        // if the number in 4th line is bigger than it should -> error 4
        //for(size_t i=0;i<m->fields;i++)
        if (biggest_bit(m->walls, div_round_up(m->fields, 8))+1 > m->fields)
            error(LAST_LINE, m);
    }
    else if (c == 'R')
    {
        // letters[0] = a, letters[1] = b, letters[2] = m, 
        //letters[3] = r, letters[4] = s_0
        size_t letters[5];
        for (int i = 0; i < 5; i++)
        {
            letters[i] = read_number_between_spaces(&c, m);
            if (letters[i] > UINT32_MAX)
                error(LAST_LINE, m);
        }
        if (letters[2] == 0)
            error(LAST_LINE, m);
        *bits = m->fields;
        make_walls(letters, m);
        c = skip_space_dont_skip_digit(c);
        if (c != EOF && c != '\n' && c != '\r')
            error(LAST_LINE, m);
        if (c != EOF && (c = getchar()) != EOF)
            error(LAST_LINE + 1, m);
    }
    // last line doesn't begin from 0 and R -> wrong input
    else
        error(LAST_LINE, m);
}


//main function to read input
maze *read() 
{
    size_t k2;
    maze *m = malloc(sizeof(maze));
    m->n = m->x = m->y = NULL;
    m->walls = NULL;
    read_line(&(m->dimensions), 1, m, &(m->n));
    if (m->dimensions == 0)
        error(1, m);
    m->fields = 1;
    for (size_t i = 0; i < m->dimensions; i++)
    {
        if (m->fields > (SIZE_MAX / (m->n)[i]))
            error(1, m);
        m->fields *= (m->n)[i];
    }
    read_line(&k2, 2, m, &(m->x));
    check_line(2, m->x, k2, m);
    read_line(&k2, 3, m, &(m->y));
    check_line(3, m->y, k2, m);
    size_t bits;
    read_last_line(&bits, m);
    return m;
}