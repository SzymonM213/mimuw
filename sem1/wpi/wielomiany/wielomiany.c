#include <stdio.h>

#define MAX_STOPIEN 10 //maksymalny stopie� wielomian�w

//funkcja zeruj�ca tablice
void zerowanie(int t[], int n) {
	for(int i=0;i<=n;i++) t[i]=0;
} 

//funkcja czytaj�ca input, omijaj�ca spacje
char niespacja() {
	char s=(char) getchar();
	while(s==' ') s= (char) getchar();
	return s;
}

//funkcja czytaj�ca sp�jn� liczb� wpisan� do programu
int liczba(char cyfra, char* znak) { 
	int n=0;
	while(cyfra>='0' && cyfra<='9') {
		n=n*10+(int) cyfra-48;
		cyfra=niespacja();
	}
	*znak=cyfra;
	return n;
}

//funkcja wczytuj�ca ca�y wielomian
void wczytywanie(int pomocnicza[]) {
    char znak; //ostatni wczytany znak
    int wsp=1; //wsp�czynnik przy x (liczba)
    znak=niespacja();
	while(znak!='\n') {
		if (znak=='-') {
			wsp=-1; //nast�pny wsp�czynnik ujemny, poniewa� zaczyna si� od minusa
			znak=niespacja();
		}
		if (znak=='+') {
			znak=niespacja(); 
			wsp=1; //nast�pny wsp�czynnik dodatni, poniewa� zavzyna si� od plusa
		}
		if (znak>='0'&& znak<='9') { //je�li znak jest cyfr�, przed kt�r� nie wyst�pi�y znaki "x^", czytamy sp�jn� liczb�, kt�rej jest cz�ci� i ustalamy warto�� wsp�czynnika
			wsp*=liczba(znak, &znak);
		}
		if (znak=='x') { //je�li wczytany znak to 'x', to albo wyst�puje po nim '^', albo wyk�adnik r�wny 1
        	char pot=niespacja();
        	if (pot=='^') {
        		znak=niespacja();
        		pomocnicza[liczba(znak, &znak)]=wsp;//dodajemy wsp�czynnik przy 'x' do indeksu jego wyk�adnika
        		wsp=0;
			}
			else { //wyk�adnik 'x' r�wny 1
				pomocnicza[1]=wsp;
				wsp=0;
				znak=pot;
			}
		}
        
    }
    pomocnicza[0]=wsp;
}

void wypisywanie(int akumulator[]) {
	int i=MAX_STOPIEN;
	while (i>=0 && akumulator[i]==0) i--; //szukamy pierwszego niezerowego wsp�czynnika (od niego zaczynamy wypisywanie)
	if (i<0) printf("0");  //skrajne przypadki
	else if (i==0) printf("%d", akumulator[0]); //skrajne przypadki
	else if (i==1) {   //skrajne przypadki
		if (akumulator[1]!=1 && akumulator[1]!=-1)printf("%dx", akumulator[1]);
		else {
			if (akumulator[1]==1) printf("x");
			else printf("-x");
		}
		if (akumulator[0]>0) printf(" + %d", akumulator[0]);
		else if (akumulator[0]<0) printf(" - %d", -akumulator[0]);
	} 
	else if (i!=0 && i!=-1) {
		if (akumulator[i]!=1 && akumulator[i]!=-1) printf("%dx^%d", akumulator[i], i);
		else { //je�li wsp�czynnik przy 'x' r�wny 1 lub -1, nie wypisujemy go
			if (akumulator[i]==1) printf("x^%d", i);
			else printf("-x^%d", i);
		}
		i--;
		while (i>=2) { //przypadki, dla kt�rych musimy wypisa� pot�g� po 'x'
			if (akumulator[i]>1) printf(" + %dx^%d", akumulator[i], i);
			else if (akumulator[i]<-1) printf(" - %dx^%d", -akumulator[i], i);
			else if (akumulator[i]==1) printf(" + x^%d", i); //je�li wsp�czynnik przy 'x' r�wny 1 lub -1, nie wypisujemy go
			else if (akumulator[i]==-1) printf(" - x^%d", i);
			i--;
		}
		//przypadki, dla kt�rych nie wypisujemy pot�gi
		if (akumulator[1]>1) printf(" + %dx", akumulator[1]); 
		else if (akumulator[1]<-1) printf(" - %dx", -akumulator[1]);
 		else if (akumulator[1]==1) printf(" + x") ; //je�li wsp�czynnik przy 'x' r�wny 1 lub -1, nie wypisujemy go
		else if (akumulator[1]==-1) printf(" - x"); 
 		if (akumulator[0]>0) printf(" + %d", akumulator[0]);
		else if (akumulator[0]<0) printf(" - %d", -akumulator[0]);
	}
}

//dodawanie wielomian�w
void dodaj(int pomocnicza[], int akumulator[]) {
	for (int i=0;i<=MAX_STOPIEN;i++) akumulator[i]+=pomocnicza[i];
	wypisywanie(akumulator);
	printf("\n");
}

//mno�enie wielomian�w
void pomnoz(int pomocnicza[], int akumulator[]) {
	for (int i=MAX_STOPIEN;i>=0;i--) {
		akumulator[i]=akumulator[i]*pomocnicza[0]; //mno�ymy ka�dy jednomian przez wyraz wolny
		for (int j=1;j<=i;j++) {
			akumulator[i]+=akumulator[i-j]*pomocnicza[j]; //mno�ymy ka�dy wyraz wielomianu akumulator przez ka�dy wyraz wielomianu pomocnicza i wynik dodajemy do wsp�czynnika przy 'x' o pot�dze r�wnej sumie pot�g 'x' przy czynnikach
		}
	}
	wypisywanie(akumulator);
	printf("\n");
}


int main (void) {
    int pomocnicza[MAX_STOPIEN+1]; //tablica pomocnicza, przechowuj�ca wsp�czynniki wielomianu, kt�rego dodajemy lub przez kt�rego mno�ymy
    int akumulator[MAX_STOPIEN+1]; //tablica g��wna, przechowuj�ca wsp�czynniki wielomianu, do kt�rego dodajemy lub (w zale�no�ci od pierwszego znaku wiersza) kt�rego wymna�amy przez wczytany wielomian
    zerowanie(pomocnicza, MAX_STOPIEN);
    zerowanie(akumulator, MAX_STOPIEN);
    char znak=(char) getchar(); //znak na pocz�tku wiersza, '+' lub '*'
    while (znak!='.') {
    	wczytywanie(pomocnicza);
    	if (znak=='+') dodaj(pomocnicza, akumulator); //je�li pocz�tkowy znak r�wny '+', dodajemy wielomian
    	else pomnoz(pomocnicza, akumulator); //je�li pocz�tkowy znak r�ny od '+', to r�wny '*', mnozymy wielomian
    	znak=niespacja(); //znak na pocz�tku wiersza, '+' lub '*'
    	zerowanie(pomocnicza, MAX_STOPIEN);
	}
    return 0;
}
