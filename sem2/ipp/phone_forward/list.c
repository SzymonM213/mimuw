/** @file
 * Implementacja funkcji używanych do wykonywania operacji na listach numerów
 *
 * @author Szymon Mrozicki
 * @date 2022
 */
#include "list.h"

List *listInit(char const *value, int length)
{
    List *l = malloc(sizeof(List));
    if (l == NULL)
        return NULL;
    l->value = string_copy(value, length);
    if (l->value == NULL)
    {
        free(l);
        return NULL;
    }
    l->next = l->prev = l;
    return l;
}

void listAdd(List **l, List *element)
{
    if (*l == NULL)
        *l = element;
    else
    {
        element->next = *l;
        element->prev = (*l)->prev;
        (*l)->prev = element;
        element->prev->next = element;
    }
}

void listDelete(List **l)
{
    while ((*l)->next != *l)
    {
        nodeDelete(l);
    }
    safe_string_free((*l)->value);
    free(*l);
    (*l) = NULL;
}

void nodeDelete(List **l)
{
    if ((*l) != NULL)
    {
        if ((*l)->next == (*l))
        {
            // lista jednoelementowa - usuwamy ją całą
            listDelete(l);
        }
        else
        {
            List *tmp = *l; // wskaźnik na element do usunięcia
            *l = (*l)->prev;
            (*l)->next = tmp->next;
            (*l)->next->prev = *l;
            safe_string_free(tmp->value);
            free(tmp);
        }
    }
}