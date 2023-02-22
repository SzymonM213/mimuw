package Agenci.Spekulanci;

import Agenci.Spekulant;
import Giełdy.HistoriaGiełdy;
import Produkty.Produkt;

import java.util.List;
import java.util.Map;

import static Produkty.Produkt.Produkty.DIAMENTY;

public class SpekulantWypukły extends Spekulant {

    public SpekulantWypukły(int id, double diamenty, int jedzenie, Map<Produkt.Produkty, List<Produkt>> produkty) {
        super(id, diamenty, jedzenie, produkty);
    }

    @Override
    protected double cenaKupna(Produkt.Produkty produkt, HistoriaGiełdy historia) {
        return historia.sredniaCenaZXDni(1, produkt)*0.9;
    }

    @Override
    protected double cenaSprzedaży(Produkt.Produkty produkt, HistoriaGiełdy historia) {
        return historia.sredniaCenaZXDni(1, produkt)*1.1;
    }

    @Override
    protected boolean czyKupuje(Produkt.Produkty produkt, HistoriaGiełdy historia) {
        if(produkt == DIAMENTY) return false;
        if(historia.getDzien() < 4) return false;
        return historia.getSredniaCena(historia.getDzien()-1)[produkt.ordinal()] +
                historia.getSredniaCena(historia.getDzien()-3)[produkt.ordinal()] >
                2*historia.getSredniaCena(historia.getDzien()-2)[produkt.ordinal()];
    }

    @Override
    protected boolean czySprzedaje(Produkt.Produkty produkt, HistoriaGiełdy historia) {
        if(produkt == DIAMENTY) return false;
        if(historia.getDzien() < 4) return false;
        return historia.getSredniaCena(historia.getDzien()-1)[produkt.ordinal()] +
                historia.getSredniaCena(historia.getDzien()-3)[produkt.ordinal()] <
                2*historia.getSredniaCena(historia.getDzien()-2)[produkt.ordinal()];
    }
}
