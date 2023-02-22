#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include "napiernaf.h"

//zwraca wiekszą z licz a, b
int max(int a, int b) {
    if(a>b) return a;
    else return b;
}

//zwraca mniejszą z liczb a, b
int min(int a, int b) {
    if(a<b) return a;
    else return b;
}

//zwraca wartość bezwzględną n
int abs(int n) {
    if(n<0) return -n;
    else return n;
}

//zwiększa tablicę 3/2 raza
int wiecej(int n) {
    //assert(n < INT_MAX / 3 * 2);
    if (n >= INT_MAX / 3 * 2) return n+1;
    else {
        if(n==1) n=2;
        return n / 2 * 3 + 1;
    }
}

void iton(int x, int **a, int *n) {
    int *wynik=NULL;
    int rozmiar = 0; //rozmiar wyniku
    int i=0;
    for(int j=0;x!=0;j++) {
        if (i == rozmiar) {
            rozmiar = wiecej(rozmiar);
            wynik = realloc(wynik, (size_t) rozmiar * sizeof *wynik);
            assert(wynik != NULL);
        }
        switch (x%4) {
            case 1:
            case -3:
                wynik[i]=j;
                i++;
                x=(x-1)/2;
                break;
            case 3:
            case -1:
                wynik[i]=-j-1;
                i++;
                if(x<INT_MAX) x=(x+1)/2;  //żeby nie wyszło poza zakres
                else {x=x/2; x++;}
                break;
            default:
                x/=2;
        }
    }
    *a=wynik;
    *n=i;
}

int ntoi(int *a, int n) {
    if(a==NULL || a[n-1]>31 || a[n-1]<-32) return 0;
    if(a[n-1]==31) {
        if(n==1 || a[n-2]>-1) return 0;
    }
    else if(a[n-1]==-32) {
        if(n>1 && a[n-2]<0) return 0;
    }
    //jak tu doszlismy to *a mieści się w int
    int x=0; //wynik
    int k=0;
    int dwadok=-1; //-2^k (minus żeby dla k=31 zmieściło się w int)
    for(int i=0;i<n;i++){
        while(k<a[i] || k+1<-a[i]) {
            k++;
            dwadok*=2;
        }
        if(a[i]>=0) x-=dwadok;
        else x+=dwadok;
    }
    return x;
}

//zwraca wykladnik 2, któremu odpowiada liczba n
int pozycja(int n) {
    if(n>=0) return n;
    else return -n-1;
}

//zwiększa tablicę o 1 miejsce
void powieksz(int **c, int *cn) {
    (*cn)++;
    *c=realloc(*c, (size_t) (*cn)*sizeof(**c));
}

//zmniejsza tablicę o 1 miejsce
void zmniejsz(int **c, int *cn) {
    (*cn)--;
    *c=realloc(*c, (size_t) (*cn)*sizeof(**c));
}

//dodaje 2^n albo -2^(-n-1) (w zaleznosci od znaku n) do c
void dodaj(int **c, int *cn, int n) {
    if(!*cn || pozycja(n)-pozycja((*c)[*cn-1])>1) {
        powieksz(c, cn);
        (*c)[*cn-1]=n;
    }
    else {
        if(pozycja(n)==pozycja((*c)[*cn-1])) {
            if((*c)[*cn-1]==n) {
                if(n>=0) (*c)[*cn-1]++;
                else  (*c)[*cn-1]--;
            }
            else zmniejsz(c, cn);
        }
        else {
            if(n>=0) {
                if((*c)[*cn-1]>=0) {
                    (*c)[*cn-1]=-(*c)[*cn-1]-1;
                    powieksz(c, cn);
                    (*c)[*cn-1]=n+1;
                }
                else (*c)[*cn-1]=-(*c)[*cn-1]-1;
            }
            else {
                if((*c)[*cn-1]>=0) (*c)[*cn-1]=-(*c)[*cn-1]-1;
                else {
                    (*c)[*cn-1]=-(*c)[*cn-1]-1;
                    powieksz(c, cn);
                    (*c)[*cn-1]=n-1; 
                }
            }
        }
    }
}

void nadd(int *a, int an, int *b, int bn, int **c, int *cn) {
    *cn=0;
    *c=NULL;
    int i=0, j=0; //indeksy, którymi przechodzimy odpowiednio po a i b
    while(i<an && j<bn) {
        if(pozycja(a[i])==pozycja(b[j])) {
            if(a[i]==b[j]) {
                if(a[i]>=0) dodaj(c, cn, a[i]+1);
                else  dodaj(c, cn, a[i]-1);
            }
            i++;
            j++;
        }
        else {
            if(pozycja(a[i])>pozycja(b[j])) {
                dodaj(c, cn, b[j]);
                j++;
            }
            else {
                dodaj(c, cn, a[i]);
                i++;
            }
        }
    }
    while(i<an) {
        dodaj(c, cn, a[i]);
        i++;
    }
    while(j<bn) {
        dodaj(c, cn, b[j]);
        j++;
    }
}

//a=a+b
void dodajdoa(int **a, int *an, int *b, int bn) {
    int *pom, pn; //tablica pomocnicza, w której przechowujemy a+b
    nadd(*a, *an, b, bn, &pom, &pn);
    free(*a);
    *a=pom;
    *an=pn;
}

//a=-a
void przeciwna(int **a, int an) {
    for(int i=0;i<an;i++) (*a)[i]=-(*a)[i]-1;
}

void nsub(int *a, int an, int *b, int bn, int **c, int *cn) {
    //odejmujemy b = dodajemy -b 
    przeciwna(&b, bn);
    nadd(a, an, b, bn, c, cn);
    przeciwna(&b, bn);
}

//a=a-b
void odejmijoda(int **a, int *an, int *b, int bn) {
    int *pom, pn; //tablica pomocnicza, w której przechowujemy a-b
    nsub(*a, *an, b, bn, &pom, &pn);
    free(*a);
    *a=pom;
    *an=pn;
}

//a=a*2^k
void pomnozprzez2dok(int *a, int an, int **pom, int *pn, int k) {
    *pom=NULL;
    *pom=realloc(*pom, (size_t) (an)*sizeof(**pom));
    *pn=an;
    for(int i=0;i<*pn;i++) {
        if(a[i]>=0) (*pom)[i]=a[i]+k;
        else (*pom)[i]=a[i]-k;
    }
}

//a staje sie a^k
void pomnozaprzez2dok(int **a, int *an, int k) {
    int *pom, pn; //tablica pomocnicza, w której przechowujemy a*2^k
    pomnozprzez2dok(*a, *an, &pom, &pn, k);
    free(*a);
    *a=pom;
    *an=pn;
}

void nmul(int *a, int an, int *b, int bn, int **c, int *cn) {
    int *pom, pn; //tablica, w której przechowujemy a*2^pozycja(b[i])
    *c=NULL;
    *cn=0;
    for(int i=0;i<bn;i++) {
        if(b[i]>=0) {
            pomnozprzez2dok(a, an, &pom, &pn, b[i]);
            dodajdoa(c, cn, pom, pn);
        }
        else {
            pomnozprzez2dok(a, an, &pom, &pn, -b[i]-1);
            odejmijoda(c, cn, pom, pn);
        }
        free(pom);
    }
}

//a=a*b
void pomnoza(int **a, int *an, int *b, int bn) {
    int *pom, pn; //tablica pomocnicza, w której przechowujemy a*b
    nmul(*a, *an, b, bn, &pom, &pn);
    free(*a);
    *a=pom;
    *an=pn;
}

//sprawdza czy a<b
int mniejsza(int *a, int an, int *b, int bn) {
    if(an<bn) return !(mniejsza(b, bn, a, an));
    //an>=bn
    if(b==NULL) return (a!=NULL && a[an-1]<0);
    int i=an-1;
    while(i>an-bn && a[i]==b[i+bn-an]) i--;
    if(a[i]!=b[i+bn-an]) return a[i]<b[i+bn-an];
    if (an>bn) return a[i-1]<0;
    return 0;
}

//a=a/2
void podzielparzystaprzezdwa(int **a, int an) {
    for(int i=0;i<an;i++) {
        if((*a)[i]>=0) (*a)[i]--;
        else (*a)[i]++;
    }
}

void ndivmodplus(int *a, int an, int *b, int bn, int **q, int *qn, int **r, int *rn) {
    *q=NULL;
    *r=NULL;
    *rn=0;
    *qn=0;
    if(an>0) *r=realloc(*r, (size_t) an*sizeof(**r));
    *rn=an;
    for(int i=0;i<an;i++) (*r)[i]=a[i]; //kopiujemy a do *r
    if(!mniejsza(a, an, b, bn)) {
        int cn=0, *c=NULL; //tablica pomocnicza, w której 
        int k;
        while(!mniejsza(*r, *rn, b, bn)) {
            k=pozycja((*r)[*rn-1])-pozycja(b[bn-1]);
            //szukamy najwiekszego k takiego, że r>=b*2^k
            pomnozprzez2dok(b, bn, &c, &cn, k);
            if(mniejsza(*r, *rn, c, cn)) {
                k--;
                podzielparzystaprzezdwa(&c, cn);
            }
            odejmijoda(r, rn, c, cn);
            dodajdoa(q, qn, &k, 1);
            free(c);
        }
    }
}

void ndivmod(int *a, int an, int *b, int bn, int **q, int *qn, int **r, int *rn) {
    int aujemne=(an==0 || a[an-1]<0);
    int bujemne=b[bn-1]<0;
    if(aujemne) przeciwna(&a, an);
    if(bujemne) przeciwna(&b, bn);
    ndivmodplus(a, an, b, bn, q, qn, r, rn);
    if(aujemne && bujemne) {
        przeciwna(&a, an);
        if(*rn) {
            int jeden=0;
            dodajdoa(q, qn, &jeden, 1);
            int *pom, pn;
            nsub(b, bn, *r, *rn, &pom, &pn);
            free(*r);
            *r=pom;
            *rn=pn;
        }
        przeciwna(&b, bn);
    }
    else {
        if(aujemne) {
            przeciwna(&a, an);
            if(*rn) {
                int *pom=NULL, pn=0;
                iton(1, &pom, &pn);
                dodajdoa(q, qn, pom, pn);
                free(pom);
                nsub(b, bn, *r, *rn, &pom, &pn);
                free(*r);
                *r=pom;
                *rn=pn;
            }
            przeciwna(q, *qn);
        }
        if(bujemne) {
            przeciwna(&b, bn);
            przeciwna(q, *qn);
        }
    }
}

//a=a/b
void podziela(int **a, int *an, int *b, int bn) {
    int *pom, pn;
    int *pom2, pn2;
    ndivmod(*a, *an, b, bn, &pom, &pn, &pom2, &pn2);
    free(*a);
    free(pom2);
    *a=pom;
    *an=pn;
}

void nexp(int *a, int an, int *b, int bn, int **c, int *cn) {
    if(a) {
        iton(1, c, cn);
        int *podziel, pn;
        iton(1, &podziel, &pn);
        int *podst=NULL;
        int pdn=an;
        podst=malloc((size_t) pdn*sizeof(*podst));
        for(int i=0;i<pdn;i++) podst[i]=a[i];
        int k=0; //do jakiej potęgi 2^k podnieśliśmy już podstawę
        for(int i=0;i<bn;i++) {
            int poz=pozycja(b[i]);
            while(poz>k) {
                k++;
                pomnoza(&podst, &pdn, podst, pdn);
            }
            if(b[i]>=0) {
                pomnoza(c, cn, podst, pdn);
                podziela(c, cn, podziel, pn);
                free(podziel);
                iton(1, &podziel, &pn);
            }
            else {
                pomnoza(&podziel, &pn, podst, pdn);
            }
        }
        free(podst);
        free(podziel);
    }
    else {
        *c=NULL;
        *cn=0;
    }
}
