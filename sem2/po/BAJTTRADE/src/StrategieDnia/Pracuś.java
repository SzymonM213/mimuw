package StrategieDnia;

import Giełdy.HistoriaGiełdy;

//nigdy się nie uczy, zawsze pracuje
public class Pracuś implements StrategiaDnia {

    @Override
    public boolean czyPracuje(double diamenty_robotnika, HistoriaGiełdy historia) {
        return true;
    }
}
