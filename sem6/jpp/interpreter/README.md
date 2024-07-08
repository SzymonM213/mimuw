## Corretto
Corretto to język wzorowany na języku Latte, z kilkoma różnicami:
- Do wypisywania wartości używa się funkcji ```printInt(int x)```, 
    ```printString(String x)```, ```printBool(bool x)```. 
- Ciała wnętrz bloków muszą być otoczone nawiasami klamrowymi
- Możliwość przekazania argumentów do funkcji przez wartość 
    (domyślnie) lub przez referencję (poprzez dodanie & przed nazwą
    parametru, np. void foo(int &x) )
- zmienne zadeklarowane wewnątrz każdego bloku nie są widoczne poza nim i przesłaniają zmienne o tej samej nazwie z zewnętrznych bloków. Wewnątrz jednego bloku nie można zadeklarować dwóch zmiennych (ani funkcji) o tej samej nazwie. Oprócz tego można zadeklarować globalne zmienne
- Zmienne mają domyślne wartości 0, false i "" odpowiednio dla typów int, bool i string
- Przed wykonaniem programu odbywa się statyczna kontrola typów
- Możliwość dowolnego zagnieżdżania bloków, w tym funkcji, funkcje zdefiniowane wewnątrz bloków nadpisują wewnątrz tego bloku funkcje o tej samej nazwie z zewnętrznych bloków
- Program zaczyna się od funkcji ```int main()```, która nie przyjmuje argumentów (i musi zwracać liczbę, która jest wynikiem działania programu). Jeżeli taka funkcja nie jest zdefiniowana, program nie przejdzie statycznej kontroli typów

## Sposób uruchomienia
Aby uruchomić program należy wywołać polecenie 'make', które zbuduje interpreter, a następnie uruchomić go poleceniem 
```bash
./interpreter <nazwa_pliku>
```
gdzie <nazwa_pliku> to nazwa pliku z kodem źródłowym w języku Corretto lub 
```bash
./interpreter
```
aby wykonać program wczytany ze standardowego wejścia.

## Przykłady
Przykłady programów znajdują się w katalogu examples, z podziałem
na poprawne (podkatalog good) i niepoprawne (podkatalog bad), pokazujące obsługę błędów.