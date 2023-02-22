# Część 1 zadania

Jako pierwszą część zadania należy zaimplementować moduł operacji przekierowywania numerów telefonów. Opis interfejsu modułu znajduje się w pliku phone_forward.h w formacie komentarzy dla programu doxygen. Przykład użycia znajduje się w pliku phone_forward_example.c.
Dostarczamy

W repozytorium https://git.mimuw.edu.pl/IPP-login.git (gdzie login to identyfikator używany do logowania się w laboratorium komputerowym) znajduje się szablon implementacji rozwiązania tego zadania. Znajdują się tam następujące pliki:

    src/phone_forward.h – deklaracja interfejsu modułu wraz z jego dokumentacją w formacie doxygen,
    src/phone_forward_example.c – przykład użycia modułu,
    CMakeLists.txt – plik konfiguracyjny programu cmake,
    Doxyfile.in – plik konfiguracyjny programu doxygen,
    MainPage.dox – strona główna dokumentacji w formacie doxygen.

Zastrzegamy sobie możliwość nanoszenia poprawek do tego szablonu. Będziemy je umieszczać w gałęzi template/part1.
Wymagamy

Jako rozwiązanie części 1 zadania wymagamy:

    ewentualnego uzupełnienia implementacji w pliku phone_forward.h,
    stworzenia pliku phone_forward.c z implementacją modułu,
    uzupełnienia dokumentacji w formacie doxygen tak, aby była przydatna dla programistów rozwijających moduł.

Implementacja funkcji phfwdReverse jest nieobowiązkowa w części 1 zadania i funkcja ta nie będzie testowana w tej części zadania. Zamieszczamy jej deklarację i opis w pliku phone_forward.h oraz przykłady jej użycia w pliku phone_forward_example.c, gdyż w kolejnych częściach zadania interfejs modułu będzie rozszerzany o kolejne funkcje, m.in. właśnie o tę funkcję.

Funkcje implementowanego modułu nie mogą gubić pamięci ani pozostawiać struktury danych w niespójnym stanie, również wtedy gdy wystąpił błąd alokowania pamięci. Funkcje muszą być odporne na błędne dane – proszę zwrócić uwagę na niebezpieczeństwa związane z użyciem funkcji strlen, strcpy itp.

Powinna być możliwość skompilowania rozwiązania w dwóch wersjach: release i debug. Wersję release kompiluje się za pomocą sekwencji poleceń:

mkdir release
cd release
cmake ..
make
make doc

Wersję debug kompiluje się za pomocą sekwencji poleceń:

mkdir debug
cd debug
cmake -D CMAKE_BUILD_TYPE=Debug ..
make
make doc

W wyniku kompilacji powinien powstać plik wykonywalny phone_forward oraz dokumentacja. W poleceniu cmake powinno być również możliwe jawne określenie wariantu release budowania pliku wynikowego:

cmake -D CMAKE_BUILD_TYPE=Release ..

Zawartość dostarczonych przez nas plików można modyfikować, o ile nie zmienia to interfejsu modułu i zachowuje wymagania podane w treści zadania, przy czym nie wolno zmieniać opcji kompilacji. Zmiany mogą dotyczyć np. stylu, dokumentacji, deklaracji typedef, włączania plików nagłówkowych, implementacji funkcji jako static inline. Ewentualne dodatkowe pliki źródłowe, będące częścią rozwiązania, należy umieścić w katalogu src. Funkcja main programu musi znajdować się w pliku phone_forward_example.c, ale zawartość tego pliku nie będzie oceniana w tej części zadania.
Oddawanie rozwiązania

Rozwiązanie należy oddawać przez wspomniane wyżej repozytorium git. W repozytorium mają się znaleźć wszystkie pliki niezbędne do zbudowania pliku wykonywalnego oraz dokumentacji. W repozytorium nie wolno umieszczać plików binarnych ani tymczasowych. W Moodle jako rozwiązanie należy umieścić tekst zawierający identyfikator finalnej wersji (ang. commit) rozwiązania, na przykład:

518507a7e9ea50e099b33cb6ca3d3141bc1d6638

Rozwiązanie należy zatwierdzić (git commit) i wysłać do repozytorium (git push) przed terminem podanym w Moodle.
Punktacja

Za w pełni poprawne rozwiązanie zadania implementujące wszystkie wymagane funkcjonalności można zdobyć maksymalnie 20 punktów. Od tej oceny będą odejmowane punkty za poniższe uchybienia:

    Za problemy ze skompilowaniem rozwiązania można stracić wszystkie punkty.
    Za każdy test, którego program nie przejdzie, traci się do 1 punktu.
    Za problemy z zarządzaniem pamięcią można stracić do 6 punktów.
    Za niezgodną ze specyfikacją strukturę plików w rozwiązaniu, niezgodne ze specyfikacją nazwy plików w rozwiązaniu lub umieszczenie w repozytorium niepotrzebnych albo tymczasowych plików można stracić do 4 punktów.
    Za złą jakość kodu, brzydki styl kodowania można stracić do 4 punktów.
    Za ostrzeżenia wypisywane przez kompilator można stracić do 2 punktów.
    Za braki w dokumentacji można stracić do 2 punktów.


# Część 2 zadania

Modyfikujemy definicję numeru telefonu. Numer telefonu jest to nadal niepusty ciąg, którego elementami są cyfry, ale teraz dozwolone są dwie dodatkowe cyfry. Cyfrę dziesięć reprezentujemy jako znak *, a cyfrę jedenaście – jako znak #.

Należy zaimplementować funkcję phfwdReverse według specyfikacji podanej w szablonie rozwiązania udostępnionym z pierwszą częścią zadania.

Ponadto w drugiej części zadania oczekujemy poprawienia ewentualnych błędów z poprzedniej części zadania.
Dostarczamy

Niczego nie dostarczamy. Rozwiązanie drugiej części zadania powinno korzystać z własnego rozwiązania części pierwszej.
Wymagamy

Obowiązują wszystkie ustalenia z pierwszej części zadania.
Oddawanie rozwiązania

Rozwiązanie należy oddawać, podobnie jak część pierwszą, przez repozytorium git. W repozytorium mają się znaleźć wszystkie pliki niezbędne do zbudowania pliku wykonywalnego oraz dokumentacji. W repozytorium nie wolno umieszczać plików binarnych ani tymczasowych. W Moodle jako rozwiązanie należy umieścić tekst zawierający identyfikator finalnej wersji (ang. commit) rozwiązania, na przykład:

518507a7e9ea50e099b33cb6ca3d3141bc1d6638

Rozwiązanie należy zatwierdzić (git commit) i wysłać do repozytorium (git push) przed terminem podanym w Moodle.
Testy

Do oceniania drugiej części zadania będą użyte wszystkie testy użyte do oceniania części pierwszej, ale niektóre testy będą zmienione lub rozszerzone. Pojawią się też nowe testy.
Punktacja

Za w pełni poprawne rozwiązanie zadania implementujące wszystkie wymagane funkcjonalności można zdobyć maksymalnie 20 punktów. Od tej oceny będą odejmowane punkty za poniższe uchybienia:

    Za problemy ze skompilowaniem rozwiązania można stracić wszystkie punkty.
    Za każdy test, którego program nie przejdzie, traci się do 1 punktu.
    Za problemy z zarządzaniem pamięcią można stracić do 6 punktów.
    Za niezgodną ze specyfikacją strukturę plików w rozwiązaniu, niezgodne ze specyfikacją nazwy plików w rozwiązaniu lub umieszczenie w repozytorium niepotrzebnych albo tymczasowych plików można stracić do 4 punktów.
    Za złą jakość kodu, brzydki styl kodowania można stracić do 4 punktów.
    Za ostrzeżenia wypisywane przez kompilator można stracić do 2 punktów.
    Za braki w dokumentacji można stracić do 2 punktów.

# Część 3 zadania

Niestety funkcja phfwdReverse nie wyznacza przeciwobrazu funkcji phfwdGet. Jeśli phfwdGet(x) = y, to x należy do wyniku phfwdReverse(y). Implikacja w drugą stronę nie zachodzi. Należy zaimplementować funkcję

PhoneNumbers * phfwdGetReverse(PhoneForward const *pf, char const *num);

która dla podanej za pomocą wskaźnika pf bazy przekierowań i podanego numeru telefonu num wyznacza posortowaną leksykograficznie listę wszystkich takich numerów telefonów i tylko takich numerów telefonów x, że phfwdGet(x) = num. Funkcja ta alokuje strukturę PhoneNumbers, która musi być zwolniona za pomocą funkcji phnumDelete. Wynikiem funkcji jest NULL, gdy nie udało się alokować pamięci. Jeśli podany napis num nie reprezentuje numeru, wynikiem jest pusty ciąg.

Należy w następujący sposób ujednolicić reakcję na parametr o wartości NULL. Wynik funkcji phfwdAdd jest false, gdy parametr pf ma wartość NULL. Funkcje phfwdDelete, phfwdRemove nic nie robią, gdy parametr pf ma wartość NULL. Wynik funkcji phfwdGet, phfwdGetReverse, phfwdReverse jest NULL, gdy parametr pf ma wartość NULL. Funkcja phnumDelete nic nie robi, gdy parametr pnum ma wartość NULL. Wynik funkcji phnumGet jest NULL, gdy parametr pnum ma wartość NULL.

Należy poprawić i uzupełnić dokumentację, aby opisywała faktyczne działanie poszczególnych operacji. W szczególności należy uwzględnić wyjaśnienia, które pojawiły się lub pojawią się na forum.

Ponadto w trzeciej części zadania oczekujemy poprawienia ewentualnych błędów z poprzednich części zadania.
Dostarczamy

Niczego nie dostarczamy. Rozwiązanie trzeciej części zadania powinno korzystać z własnego rozwiązania jego poprzednich części.
Wymagamy

Obowiązują wszystkie ustalenia z pierwszej i drugiej części zadania.
Oddawanie rozwiązania

Rozwiązanie należy oddawać, podobnie jak część pierwszą i drugą, przez repozytorium git. W repozytorium mają się znaleźć wszystkie pliki niezbędne do zbudowania pliku wykonywalnego oraz dokumentacji. W repozytorium nie wolno umieszczać plików binarnych ani tymczasowych. W Moodle jako rozwiązanie należy umieścić tekst zawierający identyfikator finalnej wersji (ang. commit) rozwiązania, na przykład:

518507a7e9ea50e099b33cb6ca3d3141bc1d6638

Rozwiązanie należy zatwierdzić (git commit) i wysłać do repozytorium (git push) przed terminem podanym w Moodle.
Testy

Do oceniania trzeciej części zadania będą użyte wszystkie testy użyte do oceniania poprzednich części, ale niektóre testy będą zmienione lub rozszerzone. Pojawią się też nowe testy.
Punktacja

Za w pełni poprawne rozwiązanie zadania implementujące wszystkie wymagane funkcjonalności można zdobyć maksymalnie 20 punktów. Od tej oceny będą odejmowane punkty za poniższe uchybienia:

    Za problemy ze skompilowaniem rozwiązania można stracić wszystkie punkty.
    Za każdy test, którego program nie przejdzie, traci się do 1 punktu.
    Za problemy z zarządzaniem pamięcią można stracić do 6 punktów.
    Za niezgodną ze specyfikacją strukturę plików w rozwiązaniu, niezgodne ze specyfikacją nazwy plików w rozwiązaniu lub umieszczenie w repozytorium niepotrzebnych albo tymczasowych plików można stracić do 4 punktów.
    Za złą jakość kodu, brzydki styl kodowania można stracić do 4 punktów.
    Za ostrzeżenia wypisywane przez kompilator można stracić do 2 punktów.
    Za braki w dokumentacji można stracić do 2 punktów.
