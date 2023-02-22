/** @file
 * Implementacja funkcji używanych do obsługi przekierowań numerów telefonu
 *
 * @author Szymon Mrozicki
 * @date 2022
 */
#include "phone_forward.h"

PhoneForward *phfwdNew(void)
{
    PhoneForward *tmp = malloc(sizeof(PhoneForward));
    if (tmp != NULL)
    {
        tmp->forwards = trieInit(NULL);
        if (tmp->forwards == NULL)
        {
            free(tmp);
            return NULL;
        }
        tmp->reverse = trieInit(NULL);
        if (tmp->reverse == NULL)
        {
            delete_tree(tmp->forwards);
            free(tmp);
            return NULL;
        }
    }
    return tmp;
}

void phfwdDelete(PhoneForward *pf)
{
    if (pf != NULL)
    {
        delete_tree(pf->forwards);
        delete_tree(pf->reverse);
        free(pf);
    }
}

/**
 * @brief Dodaje numer telefonu do ciągu numerów
 * Dodaje numer telefonu do ciągu numerów
 *
 * @param ph_nums - wskaźnik na ciąg numerów telefonu
 * @param num - dodawany numer
 * @return Wartość @p true, jeśli numer został dodany.
 *         Wartość @p false, jeśli nie udało się alokować pamięci
 */
static bool add_phone(PhoneNumbers *ph_nums, char const *num)
{
    size_t length = string_length(num);
    if (length == 0)
        return false;
    (ph_nums->size) += length;
    char const **numbers_copy = realloc(ph_nums->numbers_array, 
                (ph_nums->number_of_numbers + 1) * sizeof(char *));
    if (numbers_copy == NULL)
        return false;
    ph_nums->numbers_array = numbers_copy;
    ph_nums->numbers_array[(ph_nums->number_of_numbers)++] = num;
    return true;
}

/**
 * @brief Tworzy nową strukturę.
 * Tworzy nową strukturę niezawierającą żadnych numerów.
 *
 * @return Wskaźnik na utworzoną strukturę lub NULL, gdy nie udało się
 * alokować pamięci
 */
static PhoneNumbers *new_phone_numbers()
{
    PhoneNumbers *tmp = malloc(sizeof(PhoneNumbers));
    if (tmp == NULL)
        return NULL;
    tmp->number_of_numbers = tmp->size = 0;
    tmp->numbers_array = NULL;
    return tmp;
}

bool phfwdAdd(PhoneForward *pf, char const *num1, char const *num2)
{
    if (pf == NULL)
        return false;
    size_t length1 = string_length(num1);
    size_t length2 = string_length(num2);
    if (length1 == length2 && equal_strings(num1, num2, length1))
        return false;
    if (length1 == 0 || length2 == 0)
        return false;
    List *forward_node = listInit(num2, length2);
    if (forward_node == NULL)
    {
        return false;
    }
    List *reverse_node = listInit(num1, length1);
    if (reverse_node == NULL)
    {
        listDelete(&forward_node);
        return false;
    }
    Trie *tmp_fwd = addNode(pf->forwards, num1, forward_node, Forward);
    if (tmp_fwd == NULL)
    {
        listDelete(&reverse_node);
        safe_string_free(forward_node->value);
        free(forward_node);
        return false;
    }
    Trie *tmp_rev = addNode(pf->reverse, num2, reverse_node, Reverse);
    if (tmp_rev == NULL)
    {
        listDelete(&reverse_node);
        nodeDelete(&(tmp_fwd->node));
        return false;
    }
    forward_node->next = reverse_node;
    tmp_fwd->second_node = tmp_rev;
    tmp_rev->second_node = NULL;
    return true;
}

void phfwdRemove(PhoneForward *pf, char const *num)
{
    size_t length = string_length(num);
    if (pf != NULL)
    {
        if (length > 0)
        {
            Trie *tmp = pf->forwards;
            size_t i;
            for (i = 0; i < length && tmp != NULL; i++)
            {
                tmp = tmp->children[digit_value(num[i])];
            }
            if (i == length && tmp != NULL)
            {
                tmp->father->children[digit_value(num[length - 1])] = NULL;
                delete_tree(tmp);
            }
        }
    }
}

PhoneNumbers *phfwdGet(PhoneForward const *pf, char const *num)
{
    if (pf == NULL)
        return NULL;
    PhoneNumbers *result = new_phone_numbers();
    if (result == NULL)
        return NULL;
    size_t length = string_length(num);
    if (length == 0)
        return result;
    Trie *tmp = pf->forwards;
    char const *pref = "";
    size_t pref_index = 0;
    size_t i = 0;
    for (; i < length && tmp->children[digit_value(num[i])] != NULL; i++)
    {
        tmp = tmp->children[digit_value(num[i])];
        if (tmp->node != NULL && tmp->node->value != NULL)
        {
            pref = tmp->node->value;
            pref_index = i + 1;
        }
    }
    char *num2 = string_concat(pref, num, pref_index);
    if (num2 == NULL)
    {
        phnumDelete(result);
        return NULL;
    }
    if (!add_phone(result, num2))
    {
        free(num2);
        phnumDelete(result);
        return NULL;
    }
    return result;
}

void phnumDelete(PhoneNumbers *pnum)
{
    if (pnum != NULL)
    {
        for (size_t i = 0; i < pnum->number_of_numbers; i++)
        {
            safe_string_free(pnum->numbers_array[i]);
            pnum->numbers_array[i] = NULL;
        }
        free(pnum->numbers_array);
        pnum->numbers_array = NULL;
        free(pnum);
        pnum = NULL;
    }
}

char const *phnumGet(PhoneNumbers const *pnum, size_t idx)
{
    if (pnum == NULL || pnum->number_of_numbers <= idx)
        return NULL;
    return pnum->numbers_array[idx];
}

/**
 * @brief Usuwa duplikaty w strukturze Phonenumbers.
 *
 * Usuwa duplikaty w strukturze Phonenumbers. Funkcja alokuje pamięć w celu
 * optymalizacji złożoności.
 *
 * @return Wartość @p true, jeśli funkcja poprawnie zaalokowała pamięć i
 *         duplikaty zostały usunięte
 *         Wartość @p false, jeśli wystąpił błąd przy alokacji pamięci i nie
 *         udało się usunąć duplikatów
 */
static bool remove_duplicates(PhoneNumbers **ph_nums)
{
    // tworzymy nową strukturę, do której będziemy przepisywać
    // numery z ph_nums bez powtórzeń
    PhoneNumbers *new_ph_nums = new_phone_numbers();
    if (new_ph_nums == NULL)
        return false;
    size_t length1;
    size_t length2;
    char const *copy;
    if ((*ph_nums)->number_of_numbers > 0)
    {
        length2 = string_length(phnumGet((*ph_nums), 0));
        copy = string_copy(phnumGet((*ph_nums), 0), length2);
        if (!add_phone(new_ph_nums, copy))
        {
            safe_string_free(copy);
            phnumDelete(new_ph_nums);
            return false;
        }
    }
    for (size_t i = 1; i < (*ph_nums)->number_of_numbers; i++)
    {
        length1 = length2;
        length2 = string_length(phnumGet((*ph_nums), i));
        if (length1 != length2 || !equal_strings(phnumGet((*ph_nums), i), 
            phnumGet((*ph_nums), i - 1), length1))
        {
            copy = string_copy(phnumGet((*ph_nums), i), length2);
            if (!add_phone(new_ph_nums, copy))
            {
                safe_string_free(copy);
                phnumDelete(new_ph_nums);
                return false;
            }
        }
    }
    phnumDelete(*ph_nums);
    (*ph_nums) = new_ph_nums;
    return true;
}

PhoneNumbers *phfwdReverse(PhoneForward const *pf, char const *num)
{
    if (pf == NULL)
        return NULL;
    PhoneNumbers *result = new_phone_numbers();
    if (result == NULL)
        return NULL;
    char const *num2;
    size_t length = string_length(num);
    if (length > 0)
    {
        char const *copy = string_copy(num, length);
        if (!add_phone(result, copy))
        {
            safe_string_free(copy);
            phnumDelete(result);
            return NULL;
        }
    }
    size_t i = 0;
    Trie *tmp = pf->reverse;
    for (; i < length && tmp->children[digit_value(num[i])] != NULL; i++)
    {
        tmp = tmp->children[digit_value(num[i])];
        if (tmp->node != NULL && tmp->node->value != NULL)
        {
            List *node = tmp->node;
            do
            {
                num2 = string_concat(node->value, num, i + 1);
                if (num2 == NULL)
                {
                    phnumDelete(result);
                    return NULL;
                }
                if (!add_phone(result, num2))
                {
                    phnumDelete(result);
                    safe_string_free(num2);
                    return NULL;
                }
                node = node->next;
            } while (node != tmp->node);
        }
    }
    qsort(result->numbers_array, result->number_of_numbers, 
          sizeof(char *), string_compare);
    if (!remove_duplicates(&result))
    {
        phnumDelete(result);
        return NULL;
    }
    return result;
}

PhoneNumbers *phfwdGetReverse(PhoneForward const *pf, char const *num)
{
    PhoneNumbers *reverse_result = phfwdReverse(pf, num);
    if (reverse_result == NULL)
    {
        return NULL;
    }
    PhoneNumbers *result = new_phone_numbers();
    if (result == NULL)
    {
        phnumDelete(reverse_result);
        return NULL;
    }
    size_t num_length = string_length(num);
    for (size_t i = 0; i < reverse_result->number_of_numbers; i++)
    {
        PhoneNumbers *get = phfwdGet(pf, reverse_result->numbers_array[i]);
        if (get == NULL)
        {
            phnumDelete(reverse_result);
            phnumDelete(result);
            return NULL;
        }
        if (string_length(get->numbers_array[0]) == num_length && 
            equal_strings(get->numbers_array[0], num, num_length))
        {
            char const *copy = string_copy(reverse_result->numbers_array[i], 
                               string_length(reverse_result->numbers_array[i]));
            if (copy == NULL || !add_phone(result, copy))
            {
                safe_string_free(copy);
                phnumDelete(reverse_result);
                phnumDelete(result);
                phnumDelete(get);
                return NULL;
            }
        }
        phnumDelete(get);
    }
    phnumDelete(reverse_result);
    return result;
}