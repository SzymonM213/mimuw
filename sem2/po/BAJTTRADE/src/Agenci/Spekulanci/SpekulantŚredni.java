package Agenci.Spekulanci;

import Agenci.Spekulant;
import Giełdy.HistoriaGiełdy;
import Produkty.Produkt;

import java.util.List;
import java.util.Map;

import static Produkty.Produkt.Produkty.*;

public class SpekulantŚredni extends Spekulant {

    private int historia_spekulanta_sredniego;

    public SpekulantŚredni(int id, double diamenty, int jedzenie, Map<Produkt.Produkty, List<Produkt>> produkty, int historia_spekulanta_sredniego) {
        super(id, diamenty, jedzenie, produkty);
        this.historia_spekulanta_sredniego = historia_spekulanta_sredniego;
    }

    @Override
    protected double cenaKupna(Produkt.Produkty produkt, HistoriaGiełdy historia) {
        if (this.czyMa(produkt)) return historia.sredniaCenaZXDni(this.historia_spekulanta_sredniego, produkt)*0.9;
        else return historia.sredniaCenaZXDni(this.historia_spekulanta_sredniego, produkt)*0.95;
    }

    @Override
    protected double cenaSprzedaży(Produkt.Produkty produkt, HistoriaGiełdy historia) {
        return historia.sredniaCenaZXDni(this.historia_spekulanta_sredniego, produkt)*1.1;
    }

    @Override
    protected boolean czyKupuje(Produkt.Produkty produkt, HistoriaGiełdy historia) {
        if(produkt == DIAMENTY) return false;
        return true;
    }

    @Override
    protected boolean czySprzedaje(Produkt.Produkty produkt, HistoriaGiełdy historia) {
        if(produkt == DIAMENTY) return false;
        return true;
    }
}
