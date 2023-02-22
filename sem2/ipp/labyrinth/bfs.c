#include "utils.h"
#include "stack.h"


int visited_or_wall(size_t index, char *walls) {
  return (walls[index / 8] & 1 << (index % 8)) >> (index % 8);
}

static void visit(maze *m, size_t index, stack *s) {
  if (!visited_or_wall(index, m->walls)) {
    (m->walls)[index / 8] |= (char)(1 << index % 8);
    push(s, index, m);
  }
}

size_t bfs(size_t from, size_t to, maze *m, stack **s, size_t stack_index, size_t distance) {
  size_t from_mod, from_div = from;
  size_t nexp = 1;
  if (from == to) return distance;
  for (size_t i = 0; i < m->dimensions; i++) {
    from_mod = from_div % (m->n)[i];
    if (from_mod > 0) visit(m, from - nexp, s[!stack_index]);
    if (from_mod < (m->n)[i] - 1) visit(m, from + nexp, s[!stack_index]);
    nexp *= (m->n)[i];
    from_div = from_div / (m->n)[i];
  }
  if (empty(s[stack_index])) {
    if (empty(s[!stack_index]))
      return 0;
    else
      return bfs(pop(s[!stack_index]), to, m, s, !stack_index, distance + 1);
  } else
    return bfs(pop(s[stack_index]), to, m, s, stack_index, distance);
}