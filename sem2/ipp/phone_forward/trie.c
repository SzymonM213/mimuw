/** @file
 * Implementacja funkcji używanych do wykonywania operacji na drzewie trie
 *
 * @author Szymon Mrozicki
 * @date 2022
 */
#include "trie.h"

Trie *trieInit(char *value)
{
    Trie *t = malloc(sizeof(Trie));
    if (t == NULL)
        return NULL;
    t->father = NULL;
    if (value != NULL)
        t->node = listInit(value, string_length(value));
    else
        t->node = NULL;
    t->last_deleted_child = 0;
    for (int i = 0; i < DIGITS; i++)
    {
        t->children[i] = NULL;
    }
    return t;
}

/**
 * @brief Usuwa wierzchołek drzewa, który jest liściem
 *
 * Usuwa wierzchołek drzewa, który jest liściem
 *
 * @param t wskaźnik na usuwany liść
 */
static void delete_leaf(Trie *t)
{
    if (t->node != NULL)
    {
        if (t->node->next->prev != t->node)
        {
            // jesteśmy w drzewie forward - usuwając wartość listy musimy też
            // usunąć odpowiedni element w drzewie reverse
            delete_forward_in_reverse_trie(t);
        }
        listDelete(&(t->node));
    }
    free(t);
}

void delete_tree(Trie *t)
{
    if (t->father != NULL)
    {
        for (int i = 0; i < DIGITS; i++)
        {
            if (t->father->children[i] == t)
                t->father->children[i] = NULL;
        }
    }
    t->father = NULL;
    Trie *tmp1 = t;
    Trie *tmp2 = NULL; // będziemy tu trzymać ojca tmp1
    while (tmp1 != NULL)
    {
        // szukamy najbliższego liścia z lewej
        while (tmp1->last_deleted_child < DIGITS && tmp1->children[tmp1->last_deleted_child] == NULL)
        {
            (tmp1->last_deleted_child)++;
        }
        if (tmp1->last_deleted_child < DIGITS)
        {
            tmp2 = tmp1;
            tmp1 = tmp1->children[tmp1->last_deleted_child];
        }
        if (tmp1->last_deleted_child == DIGITS)
        {
            // wierzchołek trzymany w tmp1 jest liściem => możemy go zwolnić
            // bez wycieku pamięci
            delete_leaf(tmp1);
            // wracamy do ojca usuniętego liścia
            tmp1 = NULL;
            tmp1 = tmp2;
            if (tmp1 != NULL)
            {
                tmp2 = tmp1->father;
                (tmp1->last_deleted_child)++;
            }
        }
    }
}

Trie *addNode(Trie *t, char const *path, List *node, enum Type type)
{
    Trie *tmp = t;
    size_t length1 = string_length(path);
    Trie *free_if_no_memory = NULL;
    for (size_t i = 0; i < length1; i++)
    {
        if (tmp->children[digit_value(path[i])] == NULL)
        {
            tmp->children[digit_value(path[i])] = trieInit(NULL);
            if (tmp->children[digit_value(path[i])] == NULL)
            {
                if (free_if_no_memory != NULL)
                    delete_tree(free_if_no_memory);
                return NULL;
            }
            if (free_if_no_memory == NULL)
            {
                free_if_no_memory = tmp->children[digit_value(path[i])];
            }
            tmp->children[digit_value(path[i])]->father = tmp;
        }
        tmp = tmp->children[digit_value(path[i])];
    }
    if (type == Forward)
    {
        if (tmp->node != NULL)
        {
            delete_forward_in_reverse_trie(tmp);
            listDelete(&(tmp->node));
        }
        tmp->node = node;
    }
    else if (type == Reverse)
    {
        listAdd(&(tmp->node), node);
    }
    return tmp;
}

void delete_forward_in_reverse_trie(Trie *t)
{
    if (t->second_node->node == t->node->next)
    {
        // odpowiedni węzeł w drzewie reverse przechowuje wskaźnik na
        // listę, której początkiem jest element, który usuwamy
        if (t->node->next->next == t->node->next)
        {
            t->second_node->node = NULL;
        }
        else
            t->second_node->node = t->second_node->node->next;
    }
    nodeDelete(&(t->node->next));
    t->node->next = t->node;
}