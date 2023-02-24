# Ćwiczenie punktowane (MatrixRowSumsThreadsafe)

W rozwiązaniu zadania z poprzednich zajęć wątek, który obliczył element macierzy, przed przejściem do wiersza następnego musiał zaczekać na zsumowanie aktualnego i wypisanie sumy.

Dodatkowo, liczenie sumy zaczynało się dopiero po obliczeniu wszystkich elementów wiersza.
## Polecenie

Napisz nową wersję programu, umożliwiającą

    obliczanie elementów kolejnych wierszy bez oczekiwania na zsumowanie poprzednich,

    rozpoczęcie sumowania wiersza zanim wszystkie jego elementy zostaną obliczone.

W rozwiązaniu zastosuj bezpieczne dla wątków struktury danych.

Zadbaj o efektywność rozwiązania w przypadku, gdy liczba wierszy bardzo duża.

Pamiętaj o poprawnej obsłudze przerwań.