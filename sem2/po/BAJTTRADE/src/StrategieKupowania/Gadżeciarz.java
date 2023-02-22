package StrategieKupowania;

import Oferty.OfertaSpekulanta;
import Produkty.Produkt;
import Produkty.ProduktJakościowy;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static Produkty.Produkt.Produkty.*;

public class Gadżeciarz extends StrategiaKupowania {

    private final int liczba_narzędzi;

    public Gadżeciarz(int liczba_narzędzi) {
        this.liczba_narzędzi = liczba_narzędzi;
    }

    @Override
    public Map<Integer, Integer> poziomy_produktów(Produkt.Produkty rodzaj, int liczba_produktów, List<Produkt> programy) {
        if(rodzaj == UBRANIA || rodzaj == NARZEDZIA || rodzaj == PROGRAMY_KOMPUTEROWE) {
            Map<Integer, Integer> wynik = new HashMap<>();
            int i;
            for (i = 0; i < liczba_produktów && programy.size() > 0; i++) {
                wynik.put(((ProduktJakościowy) programy.get(0)).getPoziom_jakości(), wynik.get(((ProduktJakościowy) programy.get(0)).getPoziom_jakości()) + 1);
                programy.remove(0);
            }
            wynik.put(1, liczba_produktów - i + wynik.get(1));
            return wynik;
        }
        else return super.poziomy_produktów(rodzaj, liczba_produktów, programy);
    }

    @Override
    public List<OfertaSpekulanta> kupuj(Map<Produkt.Produkty, List<OfertaSpekulanta>> ofertySpekulantów, Double diamenty, int ubrania, int ilePotrzebujeProgramów) {
        List<OfertaSpekulanta> wynik = this.kupProdukt(ofertySpekulantów, 100, diamenty, JEDZENIE);
        wynik.addAll(this.kupProdukt(ofertySpekulantów, this.liczba_narzędzi, diamenty, NARZEDZIA));
        wynik.addAll(this.kupProdukt(ofertySpekulantów, 100-ubrania, diamenty, UBRANIA));
        wynik.addAll(this.kupProdukt(ofertySpekulantów, ilePotrzebujeProgramów, diamenty, PROGRAMY_KOMPUTEROWE));
        return wynik;
    }
}
