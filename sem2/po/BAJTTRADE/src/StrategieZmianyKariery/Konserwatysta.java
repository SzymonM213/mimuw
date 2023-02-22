package StrategieZmianyKariery;

import Giełdy.Giełda;
import Giełdy.HistoriaGiełdy;
import Oferty.Oferta;
import ŚcieżkiKariery.ŚcieżkaKariery;

import java.util.List;

//nigdy nie zmienia ścieżki kariery
public class Konserwatysta implements StrategiaZmianyŚcieżki {

    public ŚcieżkaKariery uczSię(ŚcieżkaKariery aktualnaKariera, int[] poziomyKarier, int n, HistoriaGiełdy historia) {
        poziomyKarier[aktualnaKariera.ordinal()]++;
        return aktualnaKariera;
    }

}
