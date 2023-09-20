awww - [Aplikacje WWW](https://usosweb.mimuw.edu.pl/kontroler.php?_action=katalog2/przedmioty/pokazPrzedmiot&kod=1000-214bWWW)

|project|points|
|---|---|
|mockup|10/10|
|backend|9.7/10|
|javascript|9/10|
|app improvement|9.8/10|

# Zadanie 1: makieta interfejsu aplikacji 

Duże zadania tego kursu mają doprowadzić do stworzenia aplikacji webowej, która będzie pozwalała na kompilację programów na 8-bitowe procesory. Aplikacja docelowo ma pozwalać na wprowadzanie pliku napisanego w kodzie C (przez wgranie, ale też i przez edycję), ustawianie opcji kompilatora oraz na uzyskiwanie skompilowanej treści programu. Zakładamy, że kod przeznaczony do kompilacji będzie napisany w C, a kompilacja będzie odbywać się za pomocą kompilatora SDCC


## Widoczne na obrazku pola w intencji mają mieć następującą funkcjonalność:

- Pasek menu - ma służyć na miejsce, gdzie będą pojawiały się typowe dla różnego rodzaju aplikacji menu, np. Plik, Edycja, Opcje itp. Co najmniej te trzy menu powinny być widoczne w makiecie.
- Wybór pliku - ma służyć za miejsce, gdzie będzie się wybierać oraz wstawiać pliki z kodem programów. Obszar ten powinien pozwalać na umieszczenie w nim plików oraz typowej drzewiastej struktury katalogów.
- Tekst programu - ma służyć za miejsce, gdzie będzie wyświetlany pełny kod programu, który ma zostać skompilowany.
- Fragment kodu - ma służyć za miejsce, gdzie będzie wyświetlany wybrany fragment kodu. Na późniejszych etapach działania aplikacji będziemy chcieli, aby np. wyróżnienie w obszarze tekstu programu identyfikatora procedury powodowało wyświetlenie jej kodu w obszarze fragment kodu.
- Tab1 - Tab4 - zakładki z różną zawartością. Z zakładkami tymi wiążemy taką intencję, że będą one grupowały różne kategorie opcji kompilatora, np. w jednym z tabulatorów będzie mozna wybrać procesor, dla którego kod będzie kompilowany, drugim opcje zależne od wybranego procesora, trzecim standard kodu źródłowego (np. C99 czy C11), czwartym opcje doprecyzowujące format generowanego kodu (np. czy stos ma być zewnętrzny, czy mają być generowane dane pomocnicze do profilowania itp.).
- Dane zależne od zakładki - w polu tym mają pojawiać się wartości zależne od wybranej zakładki. Na przykład po wybraniu zakładki sterującej wyborem procesora w tym miejscu powinna się pojawić możliwość wybrania jakiegoś z dostępnych procesorów.

## Oto cennik punktowy za zrealizowanie poszczególnych elementów zadania:

- HTML określający układ interfejsu zgodnie z obrazkiem i wyjaśnieniami (do 5 punktów)
        Obecne wszystkie obszary wraz z przykładową zawartością (do 1 p.)
        Wygodny układ ekranu przy rozdzielczości 1024x768 lub większej przy założeniu proporcji ekranu 4:3 oraz wygodny układ ekranu przy rozdzielczości 1280x720 lub większej przy założeniu proporcji ekranu 16:9. (do 1 p.)
        Łagodne zmiany rozmieszczenia elementów przy zmianie rozmiaru okna przeglądarki (do 1 p.)
        Poprawna i pełna struktura dokumentu HTML sprawdzona za pomocą co najmniej jednego z narzędzi, przy czym narzędzie to nie powinno zgłaszać ostrzeżeń: htmltest, tidy, html-validate, ​validator@w3.org, (do 1 p.)
        Czysta i czytelna struktura dokumentu HTML (do 1 p.)
- CSS określający wygląd aplikacji (do 5 punktów)
        Zastosowane zmienne, które pozwalają na wyświetlanie co najmniej dwóch wersji kolorystycznych strony aplikacji (do 1 p.)
        Użycie określenia medium, tak aby była dostępna wersja układu aplikacji dostosowana do urządzeń mobilnych (do 1 p.)
        Dostępne dwie wersje pliku ze stylem: czytelna dla człowieka i skompresowana (np. z użyciem narzędzia yuicompressor) (do 1 p.)
        Poprawne i pełna struktura dokumentu CSS sprawdzona za pomocą co najmniej jednego z narzędzi, przy czym narzędzie nie powinno zgłaszać ostrzeżeń, które można wyeliminować: csslint, css-validator@w3.org, stylelint (do 1 p.)
        Dobry styl pisania (DRY, czutelne konwencje nazewniczne, brak !important, komentarze wskazujące na nielokalne zależności) (do 1 p.)

# Zadanie 2: aplikacja po stronie serwera (model danych, wzorce stron, interfejs linkowo-przyciskowy)



W tej części rozwoju aplikacji należy stworzyć model danych, odpowiednie strony z wzorcami do wypełnienia oraz interfejs linkowo-przyciskowy aplikacji.

## Model danych

Należy stworzyć model danych z uwzględnieniem następujących encji:

- Katalog - encje tego rodzaju mają przechowyać informacje o plikach i innych katalogach. Oprócz opisu relacji z innymi encjami, mają:
    - nazwę,
    - opcjonalny opis,
    - datę utworzenia,
    - właściciela,
    - znacznik dostępności (fałszywy, gdy katalog został usunięty, początkowo wartość true)
    - datę zmiany znacznika dostępności,
    - datę ostatniej zmiany zawartości,
- Plik - encje tego rodzaju przechowują kod źródłowy programów. Z założenia kod źródłowy jest podzielony na znaczące sekcje. Oprócz opisu relacji z innymi encjami, encje tego rodzaju mają:
    - nazwę,
    - opcjonalny opis,
    - datę utworzenia,
    - właściciela,
    - znacznik dostępności (fałszywy, gdy katalog został usunięty, początkowo wartość true),
    - datę zmiany znacznika dostępności,
    - datę ostatniej zmiany zawartości.
- Sekcja pliku - encje tego rodzaju zawierają istotne znaczeniowo cząstki pliku lub komentarze; struktura sekcji jest złożona - sekcja może mieć podsekcje. Oprócz opisu relacji z innymi encjami, encje tego rodzaju mają:
    - opcjonalnie nazwę,
    - opcjonalny opis,
    - datę utworzenia,
    - początek sekcji,
    - koniec sekcji,
    - rodzaj sekcji,
    - status sekcji,
    - dane statusu,
    - treść.

Uwaga: do wyboru są dwa modele określania początku i końca sekcji. W pierwszym z nich sekcja zaczyna się zawsze na początku wiersza i kończy się na końcu. Wyznacznikiem początku i końca sekcji są tutaj numery wierszy. W drugim modelu początek i koniec sekcji może być w dowolnym miejscu. Wtedy miejsce te opisywane jest przez dwie wartości: number wiersza i numer znaku w wierszu. Można użyć dowolnego z tych modeli.
- Rodzaj sekcji - to encja, która określa typ zawartości sekcji; kategoria określa sposób, w jaki sekcja jest obsługiwana przez aplikację. Minimalny zestaw kategorii to: procedura, komentarz, dyrektywny kompilatora (#define, #pragma itp.), deklaracje zmiennych, wstawka asemblerowa.
- Status sekcji - to encja określająca stan sekcji; przykładowe statusy: kompiluje się z ostrzeżeniami, kompiluje się bez ostrzeżeń, nie kompiluję się.
- Dane statusu - dodatkowe dane związane ze statusem, np. błąd kompilacji, wskazanie w których linii dotyczy ostrzeżenie itp.
- Użytkownik – to encja, która określa użytkownika aplikacji. Oprócz opisu relacji z innymi encjami, encje tego rodzaju mają:
    - nazwę,
    - login,
    - hasło.

Powyższe encje oraz ich pola mogą mieć nazwy angielskojęzyczne.

Powyżej opisany model powinien mieć zrealizowaną obsługę za pomocą interfejsu użytkownika zgodnie z intencjami działania aplikacji.

## Punktacja za encje

Za tablice realizujące poszczególne encje punkty przyznawane będą według schematu:

- Użytkownik - 1 p.,
- Encje związane z plikami i katalogami - 1 p.,
- Encje związane z sekcjami - 1 p.

## Interfejs linkowo-przyciskowy i wzorce stron

Aplikacja powinna w następujący sposób udostępniać możliwość korzystania z możliwości kompilatora SDCC.

- Aplikacja powinna w sekcji Wybór pliku wyświetlać dostępną w bazie danych strukturę katalogów. Sekcja poiwnna być zaimplementowana jako odpowiedni fragment wzorca strony (lub wzorzec), który jest wypełniony danymi pochodzącymi z bazy danych.
- Aplikacja powinna umożliwiać dodanie pliku do bazy, podzielenie go na sekcje zgodnie z wymienionymi powyżej kategoriami (powinno się to dziać częściowo automatycznie, ale też powinna być możliwość określania sekcji ręcznie; w tym dodawania sekcji, oznaczania fragmentu pliku jako sekcji oraz łączenia istniejących sekcji w jedą wspólną sekcję). Operacje te powinny być dostępne w którymś z menu na pasku menu. Uwaga: pełna funkcjonalność tego typu możliwa jest do uzyskania dopiero po wprowadzeniu JavaScriptu, dlatego wystarczy, jeśli ten element pozostanie nie w pełni funkcjonalny i zrealizowana będzie jedynie część obsługi po stronie serwera.
- Aplikacja powinna umożliwiać dodanie katalogu do bazy. Operacja ta powinna być dostępna w którymś z menu na pasku menu.
- Aplikacja powinna umożliwiać skasowanie pozycji w katalogu (pliku, innego katalogu), przy czym plik czy katalog nie powinien być zupełnie usuwany z bazy danych, a jedynie należy go oznaczać jako niedostępny za pomocą znacznika dostępności. Operacja ta powinna być dostępna w którymś z menu na pasku menu oraz być połączona z odpowiednim mechanizmem wskazywania pliku lub katalogu.
- Aplikacja powinna w sekcji Fragment kodu wyświetlać zawartość pliku filename.asm uzyskanego w wyniku działania polecenia **sdcc -S filename.c** odpowiednio wzbogaconego przez inne opcje, jakie zostały określone w interfejsie użytkownika, przy czym filename.c odpowiada obecnie wybranemu plikowi. Wynik z pliku filename.asm powinien zostać podzielony na zakresy oddzielone kreskowanymi liniami. Przy czym najechanie myszką na sekcję powinno spowodować jej wyróżnienie. Osobny rodzaj wyróżnienia ma dotyczyć nagłówka sekcji, a osobny treści. Wyświetlanie tej sekcji powinno być zapewnione przez odpowiedni fragment wzorca lub wzorzec.
- W dolnej części ekranu powinny być udostępnione cztery taby. Oto opis ich zawartości
    - Pierwszy tabulator (zatytułowany STANDARD) powinien wskazywać, z jakim standardem powinna być zachowana zgodność kompilatora (co najmniej C89, C99, C11). Powinno być możliwe wybranie jednego z nich. Wtedy wszystkie wykonania kompilatora wykonywane przez aplikację powinny dotyczyć wybranego tutaj standardu.
    - Drugi tabulator (zatytułowany OPTYMALIZACJE) powinien zawierać listę dostępnych rodzajów optymalizacji (co najmniej 3). Powinna być możliwość określenia wybranego zestawu optymalizacji. Wtedy wszystkie wykonania kompilatora wykonywane przez aplikację powinny generować kod zgodnie z zestawem optymalizacji.
    - Trzeci tabulator (zatytułowany PROCESOR) powinien zawierać listę dostępnych w SDCC architektur procesorów (co najmniej MCS51, Z80 i STM8). Powinno być możliwe wybranie jednego z nich. Wtedy wszystkie wykonania kompilatora wykonywane przez aplikację powinny dotyczyć wybranego tutaj procesora.
    - Czwarty tabulator (zatytułowany ZALEŻNE) powinien zawierać listę opcji kompilatora, które są zależne od procesora. Dla każdego wybranego procesora powinna się tutaj znajdować możliwość wybrania trzech opcji właściwych dla niego (np. dla procesora MCS51 może się tutaj znajdować możliwość wyboru docelowego modelu programu - small, medium, large, huge model programs).
    - Menu powinno zawierać opcję pozwalającą na wykonanie pełnej kompilacji obecnie wskazywanego pliku. Przebieg powinien uaktualniać zawartość sekcji Fragment kodu oraz dawać możliwość zapisania wyniku kompilacji na lokalnym dysku.

## Punktacja

- Powiązanie wyboru pliku w sekcji Wybór pliku z zawartością sekcji Tekst programu - 1 p.,
- Nawigacja po katalogach i plikach, dodawanie ich i usuwanie - 1 p.,
- Automatyczny i ręczny podział pliku na sekcje - 1 p.,
- Obsługa sekcji Fragment kodu - 1 p.,
- Prawidłowa obsługa wyboru standardu - 0,5 p.
- Prawidłowa obsługa wyboru optymalizacji - 0,5 p.
- Prawidłowa obsługa wyboru procesora i opcji zależnych od niego - 1 p.
- Prawidłowa obsługa kompilacji - 1 p.

# Zadanie 3: automatyzacja po stronie przeglądarki (interakcja z programem po stronie serwera, brak przeładowywania strony)



W tej iteracji aplikacji rozszerzymy aplikację przez dodanie pewnej liczby nowych funkcjonalności; poprawimy też jej niezawodność.

## Nowe cechy rozwiązania

- Należy dodać infrastrukturę pozwalającą na logowanie użytkowników. Nie jest konieczne dodawanie funkcjonalności pozwalającej na „zapisanie” się do serwisu. Dodawanie nowych użytkowników może się odbywać przez interfejs administracyjny aplikacji. Pliki i katalogi dodawane przez danego użytkownika muszą być zapisywane w bazie jako należące do niego.
- Należy polepszyć zadowolenie użytkownika przez uniknięcie konieczności przeładowywania strony przy każdej operacji plikowej i przy każdej operacji z menu.
- Należy dodać możliwość ręcznego wprowadzenia sekcji, której treść obejmuje zaznaczony fragment pliku (jeżeli wybrany został model rozdzielania sekcji z dokładnością do wiersza, sekcja zaczyna się w wierszu, w którym jest początek zaznaczenia, a kończy w wierszu, w którym znajduje się koniec zaznaczenia).
- W przypadku nieprawidłowej kompilacji pliku należy wyświetlić informacje wypisywane przez kompilator w obszarze Fragment kodu. Informacje te zawierają wskazówki, mówiące o tym, którego wiersza kodu źródłowego dotyczy informacja. Podobne wskazówki znajdują się też w pliku *.asm. Należy sprawić pomóc użytkownikowi interfejsu przez stworzenie funkcjonalności, dzięki której po naciśnięciu na wskazówkę dotyczącą numeru wiersza w obszarze Fragment kodu – niezależnie od tego, czy kompilacja zakończyła się powodzeniem, czy porażką – wskazany wiersz w kodzie źródłowym zostanie wyróżniony. Wyróżnienie odbywać ma się na takiej zasadzie, że ten wiersz ma stać się widoczny w okienku z kodem i ma nastąpić jakieś jego dodatkowe wskazanie przez zmianę właściwości graficznych (np. kolor, właściwości czcionki) lub przez zmianę lokalizacji (np. wiersz staje się pierwszym widocznym wierszem w części okna z kodem). Po zakończeniu naciskania wskazówki wyróżnienie graficzne powinno zniknąć.
- W przypadku, gdy kompilator zakończy działanie prawidłowo, w obszarze Fragment kodu wyświetla się treść pliku *.asm. Jednak czasami sekcje tego pliku są bardzo długie. Należy wprowadzić możliwość: (a) schowania i pokazania treści wszystkich sekcji,, (b) schowania i pokazania treści pojedynczej sekcji. Nagłówki sekcji powinny być zawsze widoczne.

### Punktacja

- Dodawanie użytkowników (za pomocą panelu administratora lub w inny sposób) - 1 punkt
- Logowanie się do programu - 1 punkt
- Zmiany w plikach działają tak, że nie wykonuje się przeładowywanie strony - 1 punkt
- Wybieranie pozycji z menu nie powoduje przeładowania strony - 1 punkt
- Obsługa ręcznego dodawania sekcji do pliku - 1 punkt
- Wyróżnianie wiersza pliku źródłowego po wskazaniu informacji o numerze wiersza w sekcji Fragment kodu - 1 punkt
- Chowanie i pokazywanie zawartości sekcji - 1 punkt

## Praca nad niezawodnością aplikacji

Należy wprowadzić testy w części aplikacji po stronie serwera. Testy powinny sprawdzać modele, widoki i formularze używane w aplikacji. Aktywność z tej części będzie rozwijana w następnym zadaniu.

### Punktacja

- Testy dla wszystkich modeli - 1 punkt
- Testy dla wszystkich widoków - 1 punkt
- Testy dla wszystkich formularzy (jeśli aplikacja nie używa formularzy, to testy dla widoków powinny obejmować funkcje sprawdzające poprawność danych przesyłanych od klienta aplikacji, w takim wypadku za widoki przyznaje się do 2 punktów) - 1 punkt

# Zadanie 4: dopracowanie aplikacji lub aplikacja w innym zrębie 

Zadanie ma dwie alternatywne ścieżki wykonania. Zrealizowanie każdej pojedynczej ścieżki daje do 10 punktów. Należy wybrać jedną z tych ścieżek. Nie można dostać punktów z obydwu z nich na raz (np. scenariusz 3 punkty w pierwszej i 7 w drugiej, a więc w sumie 10 punktów, nie jest do zrealizowania). Nie należy też liczyć, że zostaną sprawdzone dwie ścieżki i dostanie się z nich maksimum. Sprawdzana będzie praca tylko w jednej z wybranych przez Was ścieżek.

## Ścieżka I - wdrożona aplikacja Django

W tej ścieżce wykonania zadania chodzi o dojście do punktu, w którym zapoznacie się z całością procesu tworzenia aplikacji WWW, łącznie z fazą jej końcowego dopracowywania szczegółów i wdrażania do działania.

### Punktacja wariantu I

- Aplikacja tworzona w ramach zadań 1-3 wdrożona z użyciem serwera nginx, apache lub innego serwera WWW o cechach produkcyjnych. (2 punkty)
- Zintegrowanie z aplikacją działania edytora kodu Codemirror. (2 punkty)
- Zmodyfikowana funkcjonalność Codemirror tak, aby wprowadzone zostało kolorowanie sekcji kodu w C, które zawierają wstawki asemblerowe. Kolorowanie to ma być niezależne od innych kolorowań w edytorze. (1 punkt)
- Przyjemny w obsłudze interfejs użytkownika, pozwalający na logowanie się korzystającego. (1 punkt)
- Pokrycie kodu testami przynajmniej na poziomie 60% (1 punkt);
    pokrycie będzie wyliczane za pomoca komendy
    **coverage run --source='source_name' manage.py test app_name**
    i pokazywane za pomocą polecenia
    **coverage report**
- Pokrycie kodu testami przynajmniej na poziomie 70% (1 punkt) mierzone, jak powyżej.
- Pokrycie kodu testami przynajmniej na poziomie 80% (1 punkt) mierzone, jak powyżej.
- Pokrycie kodu testami przynajmniej na poziomie 85% (0,5 punktu) mierzone, jak powyżej.
- Pokrycie kodu testami przynajmniej na poziomie 90% (0,5 punktu) mierzone, jak powyżej.

## Ścieżka II - aplikacja napisana w alternatywnym zrębie

W tej ścieżce wykonania zadania będziecie mogli w praktyce zapoznać się z innym zrębem tworzenia aplikacji WWW. W tej wersji zadania należy przedstawić alternatywne rozwiązanie dotychczas wykonanych zadań 1-3 z użyciem zrębu WWW innego niż Django.

Uwaga!!! Zrąb, jaki będzie użyty w tym rozwiązaniu, należy uzgodnić z prowadzącym laboratorium. Prowadzący laboratorium ma prawo nie zgodzić się na sprawdzanie rozwiązania w zaproponowanym przez Was zrębie – prowadzący nie są omnibusami i nie muszą znać wszystkich technologii tworzenia aplikacji WWW.

- Aplikacja powinna w sekcji Wybór pliku wyświetlać dostępną w bazie danych strukturę katalogów. Sekcja poiwnna być zaimplementowana jako odpowiedni fragment wzorca strony (lub wzorzec), który jest wypełniony danymi pochodzącymi z bazy danych.
-  Aplikacja powinna umożliwiać dodanie pliku do bazy, podzielenie go na sekcje zgodnie z wymienionymi we wcześniejszych zadaniach kategoriami (powinno się to dziać częściowo automatycznie, ale też powinna być możliwość określania sekcji ręcznie; w tym dodawania sekcji, oznaczania fragmentu pliku jako sekcji). Operacje te powinny być dostępne w którymś z menu na pasku menu.
- Aplikacja powinna umożliwiać dodanie katalogu do bazy. Operacja ta powinna być dostępna w którymś z menu na pasku menu.
- Aplikacja powinna umożliwiać skasowanie pozycji w katalogu (pliku, innego katalogu), przy czym plik czy katalog nie powinien być zupełnie usuwany z bazy danych, a jedynie należy go oznaczać jako niedostępny za pomocą znacznika dostępności. Operacja ta powinna być dostępna w którymś z menu na pasku menu oraz być połączona z odpowiednim mechanizmem wskazywania pliku lub katalogu.
- Aplikacja powinna w polu Fragment kodu wyświetlać zawartość pliku filename.c uzyskanego w wyniku działania polecenia
    **sdcc -S filename.c**
    odpowiednio wzbogaconego przez inne opcje, jakie zostały określone w interfejsie użytkownika, przy czym filename.c odpowiada obecnie wybranemu plikowi. Wynik z pliku filename.asm powinien zostać podzielony na zakresy oddzielone kreskowanymi liniami. Przy czym najechanie myszką na sekcję powinno spowodować jej wyróżnienie. Osobny rodzaj wyróżnienia ma dotyczyć nagłówka sekcji, a osobny treści. Wyświetlanie tej sekcji powinno być zapewnione przez odpowiedni fragment wzorca lub wzorzec.
    W dolnej części ekranu powinny być udostępnione cztery taby. Oto opis ich zawartości
    - Pierwszy tabulator (zatytułowany STANDARD) powinien wskazywać, z jakim standardem powinna być zachowana zgodność kompilatora (co najmniej C89, C99, C11). Powinno być możliwe wybranie jednego z nich. Wtedy wszystkie wykonania kompilatora wykonywane przez aplikację powinny dotyczyć wybranego tutaj standardu.
    - Drugi tabulator (zatytułowany OPTYMALIZACJE) powinien zawierać listę dostępnych rodzajów optymalizacji (co najmniej 3). Powinna być możliwość określenia wybranego zestawu optymalizacji. Wtedy wszystkie wykonania kompilatora wykonywane przez aplikację powinny generować kod zgodnie z zestawem optymalizacji.
    - Trzeci tabulator (zatytułowany PROCESOR) powinien zawierać listę dostępnych w SDCC architektur procesorów (co najmniej MCS51, Z80 i STM8). Powinno być możliwe wybranie jednego z nich. Wtedy wszystkie wykonania kompilatora wykonywane przez aplikację powinny dotyczyć wybranego tutaj procesora.
    - Czwarty tabulator (zatytułowany ZALEŻNE) powinien zawierać listę opcji kompilatora, które są zależne od procesora. Dla każdego wybranego procesora powinna się tutaj znajdować możliwość wybrania trzech opcji właściwych dla niego (np. dla procesora MCS51 może się tutaj znajdować możliwość wyboru docelowego modelu programu - small, medium, large, huge model programs).
    Menu powinno zawierać opcję pozwalającą na wykonanie pełnej kompilacji obecnie wskazywanego pliku. Przebieg powinien uaktualniać zawartość pola Fragment kodu oraz dawać możliwość zapisania wyniku kompilacji na lokalnym dysku.

### Punktacja wariantu II

- Powiązanie wyboru pliku w sekcji Wybór pliku z zawartością sekcji Tekst programu - 1 p.,
- Nawigacja po katalogach i plikach, dodawanie ich i usuwanie - 1 p.,
- Automatyczny i ręczny podział pliku na sekcje - 1 p.,
- Obsługa sekcji Fragment kodu - 1 p.,
- Prawidłowa obsługa wyboru standardu - 0,5 p.,
- Prawidłowa obsługa wyboru optymalizacji - 0,5 p.,
- Prawidłowa obsługa wyboru procesora i opcji zależnych od niego - 1 p.,
- Prawidłowa obsługa kompilacji - 1 p.,
- Brak niepotrzebnego przeładowywania strony - 1 p.,
- Ukrywanie i odkrywanie fragmentów pola Fragment kodu - 1 p.,
- Utworzenie infrastruktury do testowania aplikacji i umieszczenie w niej podstawowych testów - 1 p.