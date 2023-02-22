package Agenci.Spekulanci;

import Agenci.Spekulant;
import Giełdy.HistoriaGiełdy;
import Produkty.Produkt;

import java.util.List;
import java.util.Map;

import static Produkty.Produkt.Produkty.DIAMENTY;
import static java.lang.Math.max;

public class SpekulantRegulującyRynek extends Spekulant {

    public SpekulantRegulującyRynek(int id, double diamenty, int jedzenie, Map<Produkt.Produkty, List<Produkt>> produkty) {
        super(id, diamenty, jedzenie, produkty);
    }

    private double cena(Produkt.Produkty produkt, HistoriaGiełdy historia) {
        return historia.liczba_produktów(historia.getDzien(), produkt)/max(1, historia.liczba_produktów(historia.getDzien()-1, produkt));
    }

    @Override
    protected double cenaKupna(Produkt.Produkty produkt, HistoriaGiełdy historia) {
        return this.cena(produkt, historia)*0.9;
    }

    @Override
    protected double cenaSprzedaży(Produkt.Produkty produkt, HistoriaGiełdy historia) {
        return this.cena(produkt, historia)*1.1;
    }

    @Override
    protected boolean czyKupuje(Produkt.Produkty produkt, HistoriaGiełdy historia) {
        if(produkt == DIAMENTY) return false;
        return historia.getDzien() > 1;
    }

    @Override
    protected boolean czySprzedaje(Produkt.Produkty produkt, HistoriaGiełdy historia) {
        if(produkt == DIAMENTY) return false;
        return historia.getDzien() > 1;
    }
}
