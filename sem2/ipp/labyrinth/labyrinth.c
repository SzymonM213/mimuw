#include "bfs.h"
#include "input.h"

int main(void)
{
    maze *m=read();
    size_t from = vertex_index(m->x, m->n, m->dimensions);
    size_t to = vertex_index(m->y, m->n, m->dimensions);
    if (visited_or_wall(from, m->walls))
        error(2, m);
    if (visited_or_wall(to, m->walls))
        error(3, m);
    else if (from == to)
        printf("%d\n", 0);
    else
    {
        stack *s[2] = {malloc(sizeof(stack)), malloc(sizeof(stack))};
        init(s[0]);
        init(s[1]);
        (m->walls)[from / 8] |= (char)(1 << from % 8);
        size_t way = bfs(from, to, m, s, 0, 0);
        if (way == 0)
            printf("NO WAY\n");
        else
            printf("%lu\n", way);
        clear(s[0]);
        clear(s[1]);
        free(s[0]);
        free(s[1]);
    }
    free_maze(m);
    return 0;
}