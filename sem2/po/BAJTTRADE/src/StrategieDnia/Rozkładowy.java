package StrategieDnia;

import Giełdy.HistoriaGiełdy;

import java.util.Random;

//z prawdopodobieństwem 1 - 1/(dzien symulacji + 3) pracuje, z
//prawdopodobieństwem 1/(dzień symulacji + 3) uczy się
public class Rozkładowy implements StrategiaDnia {

    @Override
    public boolean czyPracuje(double diamenty_robotnika, HistoriaGiełdy historia) {
        Random r = new Random();
        return r.nextInt(historia.getDzien()+3) != 0;
    }
}
