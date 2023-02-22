package Giełdy;

import Oferty.OfertaRobotnika;
import Oferty.OfertaSpekulanta;
import Produkty.Produkt;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static Produkty.Produkt.Produkty.*;

public class HistoriaGiełdy {
    //listy dni, każdy dzień jest reprezentowany przez mapę, która dla każdego rodzaju przedmiotu trzyma listę
    //ofert dotyczących go
    private final List<Map<Produkt.Produkty, List<OfertaRobotnika>>> historia_ofert_robotników;
    private final List<Map<Produkt.Produkty, List<OfertaSpekulanta>>> historia_ofert_spekulantów;
    private final List<double[]> srednie_ceny;

    public HistoriaGiełdy() {
        this.historia_ofert_robotników = new ArrayList<>();
        this.historia_ofert_spekulantów = new ArrayList<>();
        this.srednie_ceny = new ArrayList<>();
    }

    public int getDzien() {
        return srednie_ceny.size();
    }

    public double[] getSredniaCena(int dzien) {
        double[] kopia = new double[this.srednie_ceny.get(dzien).length];
        for(int i = 0; i<this.srednie_ceny.get(dzien).length;i++) {
            kopia[i] = this.srednie_ceny.get(dzien)[i];
        }
        return kopia;
    }

    public double sredniaCenaZXDni(int x, Produkt.Produkty produkt) {
        double suma = 0;
        for(int i = 0;i<x;i++) {
            suma+=this.srednie_ceny.get(this.getDzien()-1-i)[produkt.ordinal()];
        }
        return suma/x;
    }

    public Produkt.Produkty najczęstszyProdukt(int dni) {
        Produkt.Produkty wynik = JEDZENIE;
        int rekord = 0;
        int licznik;
        for(var p: Produkt.Produkty.values()) {
            licznik = 0;
            for(int i=0; i<dni;i++) {
                for(var oferta: this.historia_ofert_robotników.get(this.getDzien()-i-1).get(p)) {
                    licznik+=oferta.getLiczba();
                }
                for(var oferta: this.historia_ofert_spekulantów.get(this.getDzien()-i-1).get(p)) {
                    licznik+=oferta.getLiczba();
                }
            }
            if(licznik>=rekord) {
                wynik = p;
                rekord = licznik;
            }
        }
        return wynik;
    }

    public int liczba_produktów(int dzień, Produkt.Produkty produkt) {
        int wynik = 0;
        for(var v: this.historia_ofert_robotników.get(dzień).get(produkt)) {
            wynik+=v.getLiczba();
        }
        return wynik;
    }

    public void dodajOfertę(OfertaRobotnika oferta) {
        this.historia_ofert_robotników.get(this.getDzien()).get(oferta.getProdukt().getRodzaj()).add(oferta);
    }

    public void dodajSrednieCeny(double[] srednieCeny) {
        this.srednie_ceny.add(srednieCeny);
    }

    public void dodajOfertyRobotnika(Map<Produkt.Produkty, List<OfertaRobotnika>> oferty) {
        this.historia_ofert_robotników.add(oferty);
    }

    public void dodajOfertySpekulanta(Map<Produkt.Produkty, List<OfertaSpekulanta>> oferty) {
        this.historia_ofert_spekulantów.add(oferty);
    }
}
