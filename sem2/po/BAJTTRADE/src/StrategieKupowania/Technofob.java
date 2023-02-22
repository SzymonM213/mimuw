package StrategieKupowania;

import Oferty.OfertaSpekulanta;
import Produkty.Produkt;

import java.util.List;
import java.util.Map;

import static Produkty.Produkt.Produkty.*;

public class Technofob extends StrategiaKupowania {

    public Technofob() {
        super();
    }

    @Override
    public List<OfertaSpekulanta> kupuj(Map<Produkt.Produkty, List<OfertaSpekulanta>> ofertySpekulantów, Double diamenty, int ubrania, int ilePotrzebujeProgramów) {
        return this.kupProdukt(ofertySpekulantów, 100, diamenty, JEDZENIE);
    }
}
