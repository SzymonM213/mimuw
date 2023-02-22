#ifndef STACK_H
#define STACK_H
#include <stddef.h>
#include "utils.h"

typedef struct stack
{
    size_t *elements;
    size_t size;
    size_t top;
} stack;

int empty(stack *s);

void init(stack *s);

void push(stack *s, size_t x, maze *m);

size_t pop(stack *s);

void clear(stack *s);

#endif