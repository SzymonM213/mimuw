package Agenci;

import Giełdy.HistoriaGiełdy;
import Oferty.OfertaRobotnika;
import Oferty.OfertaSpekulanta;
import Produkty.Produkt;
import Produkty.ProduktJakościowy;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static Produkty.Produkt.Produkty.*;

public abstract class Spekulant extends Agent{

    protected Spekulant(int id, double diamenty, int jedzenie, Map<Produkt.Produkty, List<Produkt>> produkty) {
        super(id, diamenty, jedzenie, produkty);
    }

    protected boolean czyMa(Produkt.Produkty produkt) {
        switch (produkt) {
            case JEDZENIE -> {
                return this.jedzenie>0;
            }
            default -> {
                return this.produkty.get(produkt).size()>0;
            }
        }
    }

    protected abstract double cenaKupna(Produkt.Produkty produkt, HistoriaGiełdy historia);

    protected abstract double cenaSprzedaży(Produkt.Produkty produkt, HistoriaGiełdy historia);

    protected abstract boolean czyKupuje(Produkt.Produkty produkt, HistoriaGiełdy historia);

    protected abstract boolean czySprzedaje(Produkt.Produkty produkt, HistoriaGiełdy historia);

    private void dodajPrzedmiot(Produkt produkt) {
        if(produkt.getRodzaj() == JEDZENIE) {
            this.jedzenie++;
        }
        else {
            this.produkty.get(produkt.getRodzaj()).add(produkt);
        }
    }

    //zwraca mapę zawierającą jako wartości oferty, które składa spekulant robotnikowi o indeksie w kluczu
    public Map<Integer, List<OfertaSpekulanta>> kupuj(Map<Produkt.Produkty, List<OfertaRobotnika>> oferty, HistoriaGiełdy historia) {
        Map<Integer, List<OfertaSpekulanta>> wynik = new HashMap<>();
        for(var v: Produkt.Produkty.values()) {
            if(this.czyKupuje(v, historia)) {
                int ileKupuje = 100;
                double cena = cenaKupna(v, historia);
                while(this.diamenty >= cena && ileKupuje > 0) {
                    oferty.get(v).get(0).kup();
                    dodajPrzedmiot(oferty.get(v).get(0).getProdukt());
                    if(wynik.get(oferty.get(v).get(0).getId_agenta()) == null) wynik.put(oferty.get(v).get(0).getId_agenta(), new ArrayList<>());
                    wynik.get(oferty.get(v).get(0).getId_agenta())
                            .add(new OfertaSpekulanta(oferty.get(v).get(0).getId_agenta(), oferty.get(v).get(0).getProdukt(), 1, cena));
                    if(oferty.get(v).get(0).getLiczba() == 0) oferty.get(v).remove(0);
                    ileKupuje--;
                    this.diamenty-=cena;
                }
            }
        }
        return wynik;
    }

    public Map<Produkt.Produkty, List<OfertaSpekulanta>> dajOferty(HistoriaGiełdy historia) {
        Map<Produkt.Produkty, List<OfertaSpekulanta>> wynik = new HashMap<>();
        for (var v : Produkt.Produkty.values()) {
            wynik.put(v, new ArrayList<>());
            if (this.czySprzedaje(v, historia)) {
                if(v == JEDZENIE) {
                    wynik.get(v).add(new OfertaSpekulanta(this.id, new Produkt(JEDZENIE), this.jedzenie, this.cenaSprzedaży(v, historia)));
                }
                else {
                    for(var produkt: this.produkty.get(v)) {
                        wynik.get(v).add(new OfertaSpekulanta(this.id, produkt, 1, this.cenaSprzedaży(v, historia)));
                    }
                }
            }
        }
        return wynik;

    }
}
