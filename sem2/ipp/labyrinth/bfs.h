#ifndef BFS_H
#define BFS_H
#include "stack.h"

size_t bfs(size_t from, size_t to, maze *m, stack **s, size_t stack_index, size_t distance);

int visited_or_wall(size_t index, char *walls);

#endif