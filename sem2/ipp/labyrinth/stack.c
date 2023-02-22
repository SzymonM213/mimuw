#include "utils.h"
#include "stack.h"

int empty(stack *s) {
    return (s->top==0);
}

void init(stack *s) {
    s->top=0;
    s->size=0;
    s->elements=NULL;
}

void push(stack *s, size_t x, maze *m) {
    if(s->top==s->size) {
        s->size=more(s->size, 0, m);
        s->elements=better_realloc(s->elements, (s->size)*sizeof(size_t), m);
    }
    (s->elements)[s->top]=x;
    (s->top)++;
}

size_t pop(stack *s) {
    (s->top)--;
    return(s->elements[s->top]);
}

void clear(stack *s) {
    free(s->elements);
}