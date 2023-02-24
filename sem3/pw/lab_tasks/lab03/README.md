# Ćwiczenie punktowane (MatrixRowSumsConcurrent)

Dany jest program przyklady03/MatrixRowSums.java.

Mamy daną klasę Matrix, która reprezentuje macierz o zadanej wielkości, której elementy oblicza funkcja definition na podstawie numeru wiersza i kolumny.

Metoda int[] rowSums() zwraca tablicę wypełnioną sumami elementów w wierszach macierzy. Metoda ta oblicza kolejne elementy macierzy sekwencyjnie.

Zakładamy, że wykonanie operacji definition może być kosztowne. Chcemy umożliwić współbieżne liczenie wielu elementów.
### Polecenie

Zaimplementuj w klasie Matrix analogiczną metodę int[] rowSumsConcurrent(), w której sumywszystkich elementów z tego samego wiersza macierzy będą liczone współbieżnie.

Wątki synchronizuj za pomocą semaforów, barier lub zasuwek.
### Wskazówka

Należy utworzyć tyle wątków pomocniczych, ile kolumn ma macierz. Każdy wątek będzie liczył elementy ze swojej kolumny we wszystkich wierszach.

Po wyznaczeniu wszystkich elementów wiersza, liczymy i wpisujemy do tablicy ich sumę, po czym przechodzimy do kolejnego wiersza.

Poprawność możesz sprawdzić, porównując tablicę wyznaczoną przez wersję współbieżną i sekwencyjną.