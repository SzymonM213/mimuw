# Ćwiczenie punktowane (Vectors)

Zdefiniuj klasę Vector, której obiekty będą reprezentowały wektory zadanej długości.

Zaimplementuj w niej metodę Vector sum(Vector other) liczącą sumę danego wektora z drugim o tej długości oraz metodę int dot(Vector other) liczącą ich iloczyn skalarny.

W obu metodach obliczenie zrealizuj wielowątkowo. Zlecaj wątkom pomocniczym dodawanie lub mnożenie fragmentów wektorów o długości 10.

Zdefiniuj metodę main() demonstrującą, na losowych danych, że dodawanie i mnożenie wektorów działa prawidłowo.

Dla sprawdzenia poprawności zdefiniuj metody sumSeq i dotSeq analogiczne do wcześniejszych, ale działające sekwencyjnie (czyli bez użycia dodatkowych wątków).

Czekanie na zakończenie pracy przez wątki pomocnicze zrealizuj za pomocą metody join(). Zadbaj o prawidłową obsługę przerwań. 