package StrategieDnia;

import Giełdy.HistoriaGiełdy;
import Produkty.Produkt;

import static java.lang.Math.min;

//uczy się zawsze wtedy i tylko wtedy, gdy stać go na kupno 100 * zapas
//jednostek jedzenia licząc po cenie równej średniej arytmetycznej średnich cen przez
//ostatnie okres dni, gdzie zapas i okres to parametry strategii.
public class Student implements StrategiaDnia {
    private final int zapas;
    private final int okres;

    public Student(int zapas, int okres) {
        this.zapas = zapas;
        this.okres = okres;
    }

    @Override
    public boolean czyPracuje(double diamenty_robotnika, HistoriaGiełdy historia) {
        double srednia_cena = 0;
        for(int i = 0;i<historia.getDzien() && i<this.okres;i++) {
            srednia_cena += historia.getSredniaCena(historia.getDzien()-1-i)[Produkt.Produkty.JEDZENIE.ordinal()];
        }
        return diamenty_robotnika < srednia_cena/(min(this.okres, historia.getDzien())) * 100 * this.zapas;
    }
}
