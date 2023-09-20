# Odwracanie permutacji

Zaimplementuj w asemblerze wołaną z języka C funkcję:

bool inverse_permutation(size_t n, int *p);

Argumentami funkcji są wskaźnik p na niepustą tablicę liczb całkowitych oraz rozmiar tej tablicy n. Jeśli tablica wskazywana przez p zawiera permutację liczb z przedziału od 0 do n-1, to funkcja odwraca tę permutację w miejscu, a wynikiem funkcji jest true. W przeciwnym przypadku wynikiem funkcji jest false, a zawartość tablicy wskazywanej przez p po zakończeniu wykonywania funkcji jest taka sama jak w momencie jej wywołania. Funkcja powinna wykrywać ewidentnie niepoprawne wartości n – patrz przykład użycia. Wolno natomiast założyć, że wskaźnik p jest poprawny.

## Oddawanie rozwiązania

Jako rozwiązanie należy wstawić w Moodle plik o nazwie inverse_permutation.asm.

## Kompilowanie

Rozwiązanie będzie kompilowane poleceniem:

nasm -f elf64 -w+all -w+error -o inverse_permutation.o inverse_permutation.asm

Rozwiązanie musi się kompilować w laboratorium komputerowym.

## Przykład użycia

Przykład użycia znajduje się w pliku inverse_permutation_example.c. Można go skompilować i skonsolidować z rozwiązaniem poleceniami:

gcc -c -Wall -Wextra -std=c17 -O2 -o inverse_permutation_example.o inverse_permutation_example.c
gcc -z noexecstack -o inverse_permutation_example inverse_permutation_example.o inverse_permutation.o

## Ocenianie

Zgodność rozwiązania ze specyfikacją będzie oceniania za pomocą testów automatycznych. Sprawdzane będą też przestrzeganie reguł ABI, poprawność odwołań do pamięci i zajętość pamięci. Należy dążyć do minimalizowania rozmiaru pamięci wykorzystywanej przez rozwiązanie. Od wyniku testów automatycznych zostanie odjęta wartość proporcjonalna do rozmiaru dodatkowej pamięci wykorzystywanej przez rozwiązanie (sekcje .bss, .data, .rodata, stos, sterta). Ponadto zostanie ustalony próg rozmiaru sekcji .text. Przekroczenie tego progu będzie skutkowało odjęciem od oceny wartości proporcjonalnej do wartości tego przekroczenia. Dodatkowym kryterium automatycznej oceny rozwiązania będzie szybkość jego działania. Rozwiązanie zbyt wolne nie uzyska maksymalnej oceny. Ocena może być obniżona za zły styl programowania. Wystawienie oceny może też być uzależnione od osobistego wyjaśnienia szczegółów działania programu prowadzącemu zajęcia.

Tradycyjny styl programowania w asemblerze polega na rozpoczynaniu etykiet od pierwszej kolumny, mnemoników od dziewiątej kolumny, a listy argumentów od siedemnastej kolumny. Inny akceptowalny styl prezentowany jest w przykładach pokazywanych na zajęciach. Kod powinien być dobrze skomentowany, co oznacza między innymi, że każda procedura powinna być opatrzona informacją, co robi, jak przekazywane są do niej parametry, jak przekazywany jest jej wynik, jakie rejestry modyfikuje. To samo dotyczy makr. Komentarza wymagają także wszystkie kluczowe lub nietrywialne linie wewnątrz procedur lub makr. W przypadku asemblera nie jest przesadą komentowanie prawie każdej linii kodu, ale należy unikać komentarzy opisujących to, co widać.