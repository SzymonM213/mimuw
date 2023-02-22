package Oferty;

import Produkty.Produkt;
import Produkty.ProduktJakościowy;

import static Produkty.Produkt.Produkty.*;

public class OfertaSpekulanta extends Oferta {

    private final double cena;

    public OfertaSpekulanta(int id_agenta, Produkt produkt, int liczba, double cena) {
        super(id_agenta, produkt, liczba);
        this.cena = cena;
    }

    public double getCena() {
        return this.cena;
    }

    public static int komparator(OfertaSpekulanta o1, OfertaSpekulanta o2) {
        if(o1.getProdukt().getRodzaj() != JEDZENIE &&
                ((ProduktJakościowy) o1.getProdukt()).getPoziom_jakości() != ((ProduktJakościowy) o1.getProdukt()).getPoziom_jakości()) {
            if(((ProduktJakościowy) o1.getProdukt()).getPoziom_jakości() > ((ProduktJakościowy) o1.getProdukt()).getPoziom_jakości()) return -1;
            else return 1;
        }
        if(o1.getCena() > o2.getCena()) return 1;
        else return -1;
    }

}
