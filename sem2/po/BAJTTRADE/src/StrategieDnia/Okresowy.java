package StrategieDnia;

import Giełdy.HistoriaGiełdy;

//uczy się co okresowosc_nauki dni, gdzie okresowosc_nauki to parametr
//strategii. W pozostałe dni pracuje. Przykładowo: dla okresowosc_nauki = 10,
//będziemy się uczyć w turze 10, 20, 30, itd
public class Okresowy implements StrategiaDnia {
    private final int okresowosc_nauki;

    public Okresowy(int okresowosc_nauki) {
        this.okresowosc_nauki = okresowosc_nauki;
    }

    @Override
    public boolean czyPracuje(double diamenty_robotnika, HistoriaGiełdy historia) {
        return historia.getDzien()%okresowosc_nauki != 0;
    }
}
