/** @file
 * Interfejs klasy przechowującej listy numerów telefonu
 *
 * @author Szymon Mrozicki
 * @date 2022
 */
#ifndef __LIST_H__
#define __LIST_H__

#include "utils.h"

/**
 * To jest struktura przechowująca listy
 */
struct List;
/**
 * Definicja struktury przechowującej listy.
 */
typedef struct List
{
    char const *value; /**< wartość elementu listy*/
    struct List *prev; /**< wskaźnik na poprzedni element*/
    struct List *next; /**< wskaźnik na następny element*/
} List;

/** @brief Tworzy nową listę.
 * Tworzy nową listę niezawierającą żadnych elementów
 * @return Wskaźnik na utworzoną listę lub NULL, gdy nie udało się
 *         alokować pamięci.
 */
List *listInit(char const *value, int length);

/** @brief Dodaje element do listy
 *  Dodaje element do listy
 *
 *  @param[in,out] l - wskaźnik na listę, do której dodajemy element
 *
 *  @param[in] element - wskaźnik na dodawany element
 */
void listAdd(List **l, List *element);

/** @brief Usuwa całą listę
 *  Usuwa całą listę i ustawia wskaźnik na NULL
 *
 *  @param[in,out] l - wskaźnik na usuwaną listę
 */
void listDelete(List **l);

/** @brief Usuwa pojedynczy element listy
 *  Usuwa pojedynczy element listy
 *
 *  @param[in,out] l - wskaźnik na usuwany element
 */
void nodeDelete(List **l);

#endif