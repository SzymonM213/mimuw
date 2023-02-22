/** @file
 * Implementacja funkcji pomocniczych
 *
 * @author Szymon Mrozicki
 * @date 2022
 */
#include "utils.h"

size_t string_length(char const *str)
{
    size_t length;
    if (str == NULL)
        return 0;
    for (length = 0; str[length] != '\0'; length++)
    {
        if (digit_value(str[length]) < 0)
            return 0;
    }
    return length;
}

bool equal_strings(char const *str1, char const *str2, size_t length)
{
    for (size_t i = 0; i < length; i++)
        if (str1[i] != str2[i])
            return false;
    return true;
}

char const *string_copy(char const *str, size_t length)
{
    char *tmp = malloc((length + 1) * sizeof(char));
    if (tmp == NULL)
        return NULL;
    for (size_t i = 0; i < length; i++)
        tmp[i] = str[i];
    tmp[length] = '\0';
    return tmp;
}

char *string_concat(char const *str1, char const *str2, size_t start2)
{
    size_t length1 = string_length(str1);
    size_t length2 = string_length(str2);
    char *result = malloc((length1 + length2 - start2 + 1) * sizeof(char));
    if (result == NULL)
    {
        return NULL;
    }
    for (size_t j = 0; j < length1; j++)
        result[j] = str1[j];
    for (size_t j = start2; j < length2; j++)
        result[j - start2 + length1] = str2[j];
    result[length1 + length2 - start2] = '\0';
    return result;
}

void safe_string_free(char const *str)
{
    if (str != NULL && str[0] != '\0')
        free((char *)str);
    str = NULL;
}

int string_compare(const void *arg1, const void *arg2)
{
    size_t i = 0;
    const char **str1 = (const char **)arg1;
    const char **str2 = (const char **)arg2;
    while ((*str1)[i] != '\0' && (*str2)[i] != '\0')
    {
        if (digit_value((*str1)[i]) > digit_value((*str2)[i]))
            return 1;
        else if (digit_value((*str1)[i]) < digit_value((*str2)[i]))
            return -1;
        i++;
    }
    if ((*str1)[i] == '\0' && (*str2)[i] == '\0')
        return 0;
    if ((*str1)[i] == '\0')
        return -1;
    return 1;
}

int digit_value(char const c)
{
    if (c >= '0' && c <= '9')
        return c - '0';
    if (c == '*')
        return 10;
    if (c == '#')
        return 11;
    return -1;
}