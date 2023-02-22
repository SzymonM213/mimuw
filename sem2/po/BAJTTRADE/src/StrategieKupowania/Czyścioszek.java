package StrategieKupowania;

import Oferty.OfertaSpekulanta;
import Produkty.Produkt;

import java.util.List;
import java.util.Map;

import static Produkty.Produkt.Produkty.JEDZENIE;
import static Produkty.Produkt.Produkty.UBRANIA;

public class Czyścioszek extends StrategiaKupowania {

    public Czyścioszek() {
        super();
    }

    @Override
    public List<OfertaSpekulanta> kupuj(Map<Produkt.Produkty, List<OfertaSpekulanta>> ofertySpekulantów, Double diamenty, int ubrania, int ilePotrzebujeProgramów) {
        List<OfertaSpekulanta> wynik = this.kupProdukt(ofertySpekulantów, 100, diamenty, JEDZENIE);
        wynik.addAll(this.kupProdukt(ofertySpekulantów, 100-ubrania, diamenty, UBRANIA));
        return wynik;
    }
}
