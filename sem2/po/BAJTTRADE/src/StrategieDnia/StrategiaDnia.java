package StrategieDnia;

import Giełdy.HistoriaGiełdy;

public interface StrategiaDnia {
    boolean czyPracuje(double diamenty_robotnika, HistoriaGiełdy historia);
}
