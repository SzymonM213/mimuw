/** @file
 * Interfejs klasy przechowującej funkcje pomocnicze
 *
 * @author Szymon Mrozicki
 * @date 2022
 */
#ifndef __UTILS_H__
#define __UTILS_H__

#include <ctype.h>
#include <limits.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include <string.h>

/**
 * @brief Sprawdza długość numeru telefonu
 *
 * Sprawdza długość numeru telefonu
 *
 * @param str numer telefonu
 * @return liczbę będącą długością numeru, jeśli ten jest poprawny
 *         lub 0 w przeciwnym przypadku
 */
size_t string_length(char const *str);

/**
 * @brief Kopiuje tablicę znaków
 *
 * Kopiuje tablicę znaków
 *
 * @param str  kopiowana tablica
 *
 * @param length długość kopiowanej tablicy
 *
 * @return kopię tablicy lub NULL, jeśli nie udało się zaalokować pamięci
 */
char const *string_copy(char const *str, size_t length);

/**
 * @brief Sprawdza czy 2 wyrazy o tej samej długości są równe
 *
 * Sprawdza czy 2 wyrazy o tej samej długości są równe
 *
 * @param str1 pierwszy porównywany wyraz
 * @param str2 drugi porównywany wyraz
 * @param length długość obu wyrazów
 * @return true jeżeli wyrazy są równe
 * @return false w przeciwnym przypadku
 */
bool equal_strings(char const *str1, char const *str2, size_t length);

/**
 * @brief Skleja pierwszy wyraz z sufiksem drugiego wyrazu
 *
 * Sprawdza czy 2 wyrazy o tej samej długości są równe
 *
 * @param str1 pierwszy wyraz
 * @param str2 drugi wyraz
 * @param start2 początek sufiksu drugiego wyrazu
 * @return konkatenację obu wyrazów lub NULL, jeśli nie udało się zaalokować
 *         pamięci
 */
char *string_concat(char const *str1, char const *str2, size_t start2);

/**
 * @brief Bezpieczne zwalnianie wskaźnika, aby nie zwolnić pustego wyrazu
 *
 * Bezpieczne zwalnianie wskaźnika, aby nie zwolnić pustego wyrazu
 *
 * @param str zwalniany wskaźnik
 */
void safe_string_free(char const *str);

/**
 * @brief Funkcja porównująca dwa numery
 *
 * Funkcja porównująca dwa numery wykorzystywana do sortowania tablicy numerów
 *
 * @param arg1 pierwszy numer
 * @param arg2 drugi numer
 *
 * @return 1, jeśli pierwszy numer jest większy od drugiego
 * @return -1, jeśli drugi numer jest większy od pierwszego
 * @return 0, jeśli numery są równe
 */
int string_compare(const void *arg1, const void *arg2);

/**
 * @brief Zwraca wartość liczbową cyfry
 *
 * @param c znak reprezentujący cyfrę
 * @return wartość cyfry, jeśli znak którąś reprezentuje
 * @return -1, jeśli znak nie reprezentuje żadnej cyfry
 */
int digit_value(char const c);

#endif