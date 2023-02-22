/** @file
 * Interfejs klasy przechowującej prefiksowe drzewa przekierowań
 *
 * @author Szymon Mrozicki
 * @date 2022
 */
#ifndef __TRIE_H__
#define __TRIE_H__

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include "list.h"

/**
 * Stała reprezentująca liczbę znaków mogących występować w numerze
 */
#define DIGITS 12

/**
 * Enumerator mówiący o rodzaju drzewa
 */
enum Type
{
    Forward,
    Reverse
};

/**
 * To jest struktura przechowująca prefiksowe drzewo przekierowań.
 */
struct Trie;
/**
 * Definicja struktury przechowującej prefiksowe drzewo przekierowań.
 */
typedef struct Trie
{
    struct Trie *father;           /**< wskaźnik na ojca wierzchołka*/
    struct Trie *children[DIGITS]; /**< tablica dzieci wierzchołka.
                                    * ciąg indeksów, do których wchodzimy
                                    * podczas podróży do wierzchołka
                                    * reprezentuje prefiks, który jest
                                    * przekierowywany*/
    struct Trie *second_node;      /**< Wskaźnik na element w drzewie reverse
                                    * odpowiadający danemu elementowi z drzewa
                                    * forward (w drzewie reverse ten wskaźnik
                                    * jest NULLem*/
    List *node;                    /**< Wartość trzymana w węźle.
                                    * W zależności od rodzaju drzewa:
                                    * w przypadku drzewa forward na początku
                                    * listy trzymamy wartość przekierowania,
                                    * a w następnym elemencie trzymamy wskaźnik
                                    * na prefiks odpowiadający temu
                                    * przekierowaniu trzymany w drzewie reverse.
                                    * Ułatwia to usuwanie przekierowań.
                                    * W przypadku drzewa reverse trzymamy po
                                    * prostu listę numerów, które powinniśmy
                                    * uwzględnić przy wykonywaniu phfwdReverse
                                    * z argumentem o prefiksie będącym
                                    * reprezentacją drogi do tego wierzchołka.
                                    */
    int last_deleted_child;        /**< zmienna przydatna do szybszego usuwania
                                    * drzewa, przechowuje indeks ostatniego
                                    * usuniętego dziecka*/
} Trie;

/** @brief Tworzy wierzchołek drzewa o wartości value.
 * Tworzy wierzchołek drzewa o wartości value. Wartość ta odpowiada
 * przekierowaniu prefiksu będącego liczbową
 * reprezentacją ścieżki do stworzonego wierzchołka.
 * @return Wskaźnik na utworzoną strukturę lub NULL, gdy nie udało się
 *         alokować pamięci.
 */
Trie *trieInit(char *value);

/**
 * @brief Usuwa poddrzewo o podanym korzeniu
 *
 * Usuwa poddrzewo o podanym korzeniu
 *
 * @param t - korzeń usuwanego drzewa
 */
void delete_tree(Trie *t);

/**
 * @brief Dodaje do drzewa wartość node
 *
 * Dodaje do drzewa wartość node
 *
 * @param t - korzeń drzewa, do którego dodajemy
 * @param path - ścieżka, która prowadzi do dodanej wartości
 * @param node - dodawana wartość
 * @param type - typ drzewa - w zależności od typu albo dodajemy
 *               wartość do listy, która już istnieje w odpowiednim węźle,
 *               albo zastępujemy istniejącą tam listę nową, jednoelementową
 *
 * @return wskaźnik na węzeł, do którego prowadzi @p path
 */
Trie *addNode(Trie *t, char const *path, List *node, enum Type type);

/**
 * @brief Funkcja pomocnicza, sprzątająca drzewo reverse przy usuwaniu
 *        pojedynczego przekierowania
 *
 * Funkcja pomocnicza, sprzątająca drzewo reverse przy usuwaniu
 * pojedynczego przekierowania
 *
 * @param t - wskaźnik na węzeł, którego przekierowanie usuwamy
 */
void delete_forward_in_reverse_trie(Trie *t);

#endif