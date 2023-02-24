# Ćwiczenie punktowane (MatrixRowSumsExecutors)

W rozwiązaniach zadań z poprzednich laboratoriów mieliśmy po jednym wątku liczącym elementy macierzy dla każdej jej kolumny.

Dla macierzy o dużej liczbie kolumn, koszt tworzenia i zarządzania wątkami w takim programie byłby znaczny.
## Polecenie

Napisz nową wersję programu, w której elementy macierzy są liczone przez czteroelementową pulę wątków. Obliczenie każdego elementu macierzy powinno być osobnym zleceniem dla puli. Sumowanie może się odbywać w wątku głównym.