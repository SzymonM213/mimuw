#include <stdio.h>

#ifndef POLA
#define POLA 5
#endif

#ifndef WIERSZE
#define WIERSZE 26
#endif

#ifndef KOLUMNY
#define KOLUMNY 26
#endif

#ifndef WYBOR
#define WYBOR 1000
#endif

int min(int a, int b) {
    if(a>b) return a;
    else return b;
}

int max(int a, int b) {
    if(a<b) return a;
    else return b;
}

void zerowanie(int plansza[][KOLUMNY], int wypelnienie) { //nadaje kazdemu elementowi tablicy o rozmiarach WIERSZE x KOLUMNY wartosc wypelnienie
    for(int i=0;i<WIERSZE;i++) {
        for(int j=0;j<KOLUMNY;j++) {
            plansza[i][j]=wypelnienie;
        }
    }
}

void wypisz(int plansza[][KOLUMNY]) { //wypisuje planszę (w sumie niepotrzebne, ale zostawiłem bo się przydaje przy testowaniu)
    for(int i=0;i<WIERSZE;i++) {
        for(int j=0;j<KOLUMNY;j++) {
            printf("%d ", plansza[i][j]);
        }
        printf("\n");
    }
}

void wstaw_kloca(int plansza[][KOLUMNY], char gracz, int kolumna, int wiersz) {
    if (gracz=='l') {
        for(int i=0;i<POLA;i++) plansza[wiersz+i][kolumna]=1;
    }
    else for(int i=0;i<POLA;i++) plansza[wiersz][kolumna+i]=1;
}

void odstaw_kloca(int plansza[][KOLUMNY], int kolumna, int wiersz) { 
    for(int i=0;i<POLA;i++) plansza[wiersz][kolumna+i]=0;
}

int ocena(int plansza[][KOLUMNY], int kolumna, int wiersz) { //ocena planszy po wstawieniu klocka w miejsce (kolumna, wiersz)
    wstaw_kloca(plansza, 'p', kolumna, wiersz);
    int ocena_poz=0;
    int suma;
    int j;
    for(int i=0;i<WIERSZE;i++) {
        j=0;
        while(j<=KOLUMNY-POLA) {
            suma=0;
            for(int k=0;k<POLA;k++) suma+=plansza[i][j+k];
            if (suma==0) {
                ocena_poz+=1;
                j+=POLA-1;
            }
            j++;
        }
    }
    for(int i=0;i<KOLUMNY;i++) {
        j=0;
        while(j<=WIERSZE-POLA) {
            suma=0;
            for(int k=0;k<POLA;k++) suma+=plansza[j+k][i];
            if (suma==0) {
                ocena_poz-=1;
                j+=POLA-1;
            }
            j++;
        }
    }
    odstaw_kloca(plansza, kolumna, wiersz);
    return ocena_poz;
}

void ruch(int plansza[][KOLUMNY], int *koniec) { //wykonujemy ruch
    int ruchy[WIERSZE][KOLUMNY]; //zapisuje pozycję po wstawieniu klocka na pole (i, j)
    int maks=-1000; //zapisuje wartość maksymalnej oceny pozycji (nigdy nie będzie mniejsza od 1000 bo maks rozmiaty tablicy to 26x26)
    int licz_maksy=1;
    zerowanie(ruchy, -1000);
    int suma;
    for(int i=0;i<WIERSZE;i++){
        for(int j=0;j<=KOLUMNY-POLA;j++) {
            suma=0;
            for(int k=j;k<j+POLA;k++) suma+=plansza[i][k];
            if(suma==0) {
                ruchy[i][j]=ocena(plansza, j, i);
                if(ruchy[i][j]>maks) {
                    maks=ruchy[i][j];
                    licz_maksy=1;
                }
                else if(ruchy[i][j]==maks) licz_maksy++;
            }
            else ruchy[i][j]=-1000;
        }
    }
    if(maks>-1000) {
        int wybor_ruchu=WYBOR%licz_maksy; //numer optymalnego ruchu, który wybieramy
        int i=-1, j=0;
        int licznik=-1;
        while (licznik<wybor_ruchu) {
            i++;
            j=0;
            while(licznik<wybor_ruchu && j<KOLUMNY) {
                if(ruchy[i][j]==maks) licznik++;
                j++;
            }
        }
        j--;
        wstaw_kloca(plansza, 'p', j, i);
        char w=(char) j+'A';
        char k=(char) i+'a';
        printf("%c%c\n", k, w);
    }
    else {
        printf(".\n");
        *koniec=1; //poddajemy się
    }
}



int main(void) {
    int plansza[WIERSZE][KOLUMNY];
    zerowanie(plansza, 0);
    int kolumna=getchar()-'A';
    int wiersz=0;
    int koniec=0; //zmienna mówiąca czy kończymy program
    if(kolumna=='-'-'A') {
        ruch(plansza, &koniec);
        getchar();
        if(!koniec) kolumna=getchar()-'A';
    }
    if(!koniec) {
        if(kolumna=='.'-'A') koniec=1;
        else wiersz=getchar()-'a';
        getchar();
    }

    while(!koniec) {
        wstaw_kloca(plansza, 'l', kolumna, wiersz);
        ruch(plansza, &koniec);
        if (!koniec) {
            kolumna = getchar() - 'A';
            if(kolumna=='.'-'A') koniec=1;
            else wiersz = getchar() - 'a';
            getchar();
        }
    }
    return 0;
}
