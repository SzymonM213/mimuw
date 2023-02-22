package StrategieKupowania;

import Oferty.OfertaSpekulanta;
import Produkty.Produkt;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public abstract class StrategiaKupowania {

    protected StrategiaKupowania() {}

    public Map<Integer, Integer> poziomy_produktów(Produkt.Produkty rodzaj, int liczba_produktów, List<Produkt> programy) {
        Map<Integer, Integer> wynik = new HashMap<>();
        wynik.put(1, liczba_produktów);
        return wynik;
    }

    public List<OfertaSpekulanta> kupProdukt(Map<Produkt.Produkty, List<OfertaSpekulanta>> ofertySpekulantów,
                                                       int ileKupuje, Double diamenty, Produkt.Produkty rodzaj) {
        List<OfertaSpekulanta> wynik = new ArrayList<>();
        while(ileKupuje>0 && diamenty > ofertySpekulantów.get(rodzaj).get(0).getCena()) {
            wynik.add(new OfertaSpekulanta(ofertySpekulantów.get(rodzaj).get(0).getId_agenta(), ofertySpekulantów.get(rodzaj).get(0).getProdukt(), 1,
                    ofertySpekulantów.get(rodzaj).get(0).getCena()));
            ileKupuje--;
            diamenty-=ofertySpekulantów.get(rodzaj).get(0).getCena();
            ofertySpekulantów.get(rodzaj).get(0).kup();
            if(ofertySpekulantów.get(rodzaj).get(0).getLiczba() == 0) ofertySpekulantów.get(rodzaj).remove(0);
        }
        return wynik;
    }

    public abstract List<OfertaSpekulanta> kupuj(Map<Produkt.Produkty, List<OfertaSpekulanta>> ofertySpekulantów, Double diamenty, int ubrania, int ilePotrzebujeProgramów);
}
