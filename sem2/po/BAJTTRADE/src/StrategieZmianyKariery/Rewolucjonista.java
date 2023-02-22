package StrategieZmianyKariery;

import Giełdy.HistoriaGiełdy;
import ŚcieżkiKariery.ŚcieżkaKariery;

//raz na 7 dni oblicza n jako max(1, swoje id modulo 17). Następnie
//wybiera ścieżkę kariery dającą premię odpowiadającą produktowi, który pojawił się
//najczęściej (w sensie sumarycznej liczby jednostek w ofertach sprzedaży
//Spekulantów i Robotników) przez ostanie n dni. W pozostałe dni nie zmienia ścieżki
//kariery. Jeżeli w powyższy sposób wybrał ścieżkę kariery, którą aktualnie posiada to
//zamiast tego się w niej rozwija. Przykładowo: w dniach 1-6 nie zmienia ścieżki, a w 7
//turze wybiera potencjalnie nową ścieżkę kariery. Potem w turach 8-13 nie zmienia
//ścieżki, i w 14 wybiera potencjalnie nową, itd
public class Rewolucjonista implements StrategiaZmianyŚcieżki {
    public ŚcieżkaKariery uczSię(ŚcieżkaKariery aktualnaKariera, int[] poziomyKarier, int n, HistoriaGiełdy historia) {
        if(historia.getDzien()%7!=0) {
            poziomyKarier[aktualnaKariera.ordinal()]++;
            return aktualnaKariera;
        }
        return historia.najczęstszyProdukt(n).getKariera();
    }
}
