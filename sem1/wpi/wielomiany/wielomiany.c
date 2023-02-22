#include <stdio.h>

#define MAX_STOPIEN 10 //maksymalny stopieñ wielomianów

//funkcja zeruj¹ca tablice
void zerowanie(int t[], int n) {
	for(int i=0;i<=n;i++) t[i]=0;
} 

//funkcja czytaj¹ca input, omijaj¹ca spacje
char niespacja() {
	char s=(char) getchar();
	while(s==' ') s= (char) getchar();
	return s;
}

//funkcja czytaj¹ca spójn¹ liczbê wpisan¹ do programu
int liczba(char cyfra, char* znak) { 
	int n=0;
	while(cyfra>='0' && cyfra<='9') {
		n=n*10+(int) cyfra-48;
		cyfra=niespacja();
	}
	*znak=cyfra;
	return n;
}

//funkcja wczytuj¹ca ca³y wielomian
void wczytywanie(int pomocnicza[]) {
    char znak; //ostatni wczytany znak
    int wsp=1; //wspó³czynnik przy x (liczba)
    znak=niespacja();
	while(znak!='\n') {
		if (znak=='-') {
			wsp=-1; //nastêpny wspó³czynnik ujemny, poniewa¿ zaczyna siê od minusa
			znak=niespacja();
		}
		if (znak=='+') {
			znak=niespacja(); 
			wsp=1; //nastêpny wspó³czynnik dodatni, poniewa¿ zavzyna siê od plusa
		}
		if (znak>='0'&& znak<='9') { //jeœli znak jest cyfr¹, przed któr¹ nie wyst¹pi³y znaki "x^", czytamy spójn¹ liczbê, której jest czêœci¹ i ustalamy wartoœæ wspó³czynnika
			wsp*=liczba(znak, &znak);
		}
		if (znak=='x') { //jeœli wczytany znak to 'x', to albo wystêpuje po nim '^', albo wyk³adnik równy 1
        	char pot=niespacja();
        	if (pot=='^') {
        		znak=niespacja();
        		pomocnicza[liczba(znak, &znak)]=wsp;//dodajemy wspó³czynnik przy 'x' do indeksu jego wyk³adnika
        		wsp=0;
			}
			else { //wyk³adnik 'x' równy 1
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
	while (i>=0 && akumulator[i]==0) i--; //szukamy pierwszego niezerowego wspó³czynnika (od niego zaczynamy wypisywanie)
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
		else { //jeœli wspó³czynnik przy 'x' równy 1 lub -1, nie wypisujemy go
			if (akumulator[i]==1) printf("x^%d", i);
			else printf("-x^%d", i);
		}
		i--;
		while (i>=2) { //przypadki, dla których musimy wypisaæ potêgê po 'x'
			if (akumulator[i]>1) printf(" + %dx^%d", akumulator[i], i);
			else if (akumulator[i]<-1) printf(" - %dx^%d", -akumulator[i], i);
			else if (akumulator[i]==1) printf(" + x^%d", i); //jeœli wspó³czynnik przy 'x' równy 1 lub -1, nie wypisujemy go
			else if (akumulator[i]==-1) printf(" - x^%d", i);
			i--;
		}
		//przypadki, dla których nie wypisujemy potêgi
		if (akumulator[1]>1) printf(" + %dx", akumulator[1]); 
		else if (akumulator[1]<-1) printf(" - %dx", -akumulator[1]);
 		else if (akumulator[1]==1) printf(" + x") ; //jeœli wspó³czynnik przy 'x' równy 1 lub -1, nie wypisujemy go
		else if (akumulator[1]==-1) printf(" - x"); 
 		if (akumulator[0]>0) printf(" + %d", akumulator[0]);
		else if (akumulator[0]<0) printf(" - %d", -akumulator[0]);
	}
}

//dodawanie wielomianów
void dodaj(int pomocnicza[], int akumulator[]) {
	for (int i=0;i<=MAX_STOPIEN;i++) akumulator[i]+=pomocnicza[i];
	wypisywanie(akumulator);
	printf("\n");
}

//mno¿enie wielomianów
void pomnoz(int pomocnicza[], int akumulator[]) {
	for (int i=MAX_STOPIEN;i>=0;i--) {
		akumulator[i]=akumulator[i]*pomocnicza[0]; //mno¿ymy ka¿dy jednomian przez wyraz wolny
		for (int j=1;j<=i;j++) {
			akumulator[i]+=akumulator[i-j]*pomocnicza[j]; //mno¿ymy ka¿dy wyraz wielomianu akumulator przez ka¿dy wyraz wielomianu pomocnicza i wynik dodajemy do wspó³czynnika przy 'x' o potêdze równej sumie potêg 'x' przy czynnikach
		}
	}
	wypisywanie(akumulator);
	printf("\n");
}


int main (void) {
    int pomocnicza[MAX_STOPIEN+1]; //tablica pomocnicza, przechowuj¹ca wspó³czynniki wielomianu, którego dodajemy lub przez którego mno¿ymy
    int akumulator[MAX_STOPIEN+1]; //tablica g³ówna, przechowuj¹ca wspó³czynniki wielomianu, do którego dodajemy lub (w zale¿noœci od pierwszego znaku wiersza) którego wymna¿amy przez wczytany wielomian
    zerowanie(pomocnicza, MAX_STOPIEN);
    zerowanie(akumulator, MAX_STOPIEN);
    char znak=(char) getchar(); //znak na pocz¹tku wiersza, '+' lub '*'
    while (znak!='.') {
    	wczytywanie(pomocnicza);
    	if (znak=='+') dodaj(pomocnicza, akumulator); //jeœli pocz¹tkowy znak równy '+', dodajemy wielomian
    	else pomnoz(pomocnicza, akumulator); //jeœli pocz¹tkowy znak ró¿ny od '+', to równy '*', mnozymy wielomian
    	znak=niespacja(); //znak na pocz¹tku wiersza, '+' lub '*'
    	zerowanie(pomocnicza, MAX_STOPIEN);
	}
    return 0;
}
