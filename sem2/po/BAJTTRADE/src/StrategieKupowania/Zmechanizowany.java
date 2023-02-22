package StrategieKupowania;

import Oferty.OfertaSpekulanta;
import Produkty.Produkt;

import java.util.List;
import java.util.Map;

import static Produkty.Produkt.Produkty.*;

public class Zmechanizowany extends StrategiaKupowania {

    private final int liczba_narzędzi;

    public Zmechanizowany(int liczba_narzędzi) {
        this.liczba_narzędzi = liczba_narzędzi;
    }

    @Override
    public List<OfertaSpekulanta> kupuj(Map<Produkt.Produkty, List<OfertaSpekulanta>> ofertySpekulantów, Double diamenty, int ubrania, int ilePotrzebujeProgramów) {
        List<OfertaSpekulanta> wynik = this.kupProdukt(ofertySpekulantów, 100, diamenty, JEDZENIE);
        wynik.addAll(this.kupProdukt(ofertySpekulantów, this.liczba_narzędzi, diamenty, NARZEDZIA));
        wynik.addAll(this.kupProdukt(ofertySpekulantów, 100-ubrania, diamenty, UBRANIA));
        return wynik;
    }
}
