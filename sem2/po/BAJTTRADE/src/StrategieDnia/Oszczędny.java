package StrategieDnia;

import Giełdy.HistoriaGiełdy;

// uczy się wtedy i tylko wtedy, gdy ma powyżej limit_diamentów
// diamentów, gdzie limit_diamentów to parametr strategii
public class Oszczędny implements StrategiaDnia {
    private final int limit_diamentów;

    public Oszczędny(int limit_diamentów) {
        this.limit_diamentów = limit_diamentów;
    }

    @Override
    public boolean czyPracuje(double diamenty_robotnika, HistoriaGiełdy historia) {
        if(diamenty_robotnika>this.limit_diamentów) return false;
        return true;
    }
}
