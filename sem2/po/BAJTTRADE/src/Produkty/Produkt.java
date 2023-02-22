package Produkty;

import ŚcieżkiKariery.ŚcieżkaKariery;

import static Produkty.Produkt.Produkty.*;
import static ŚcieżkiKariery.ŚcieżkaKariery.*;

public class Produkt {
    public enum Produkty {
        JEDZENIE, UBRANIA, NARZEDZIA, DIAMENTY, PROGRAMY_KOMPUTEROWE;

        public ŚcieżkaKariery getKariera() {
            switch (this) {
                case UBRANIA -> {
                    return RZEMIEŚLNIK;
                }
                case JEDZENIE -> {
                    return ROLNIK;
                }
                case NARZEDZIA -> {
                    return INŻYNIER;
                }
                case DIAMENTY -> {
                    return GÓRNIK;
                }
                default -> {
                    return PROGRAMISTA;
                }
            }
        }

    }
    private final Produkty rodzaj;

    public Produkt(Produkty rodzaj) {
        this.rodzaj = rodzaj;
    }

    public static Produkt produkt(Produkty rodzaj, int poziom_jakości) {
        if(rodzaj == UBRANIA || rodzaj == NARZEDZIA || rodzaj == PROGRAMY_KOMPUTEROWE) {
            return new ProduktJakościowy(rodzaj, poziom_jakości);
        }
        else return new Produkt(rodzaj);
    }

    public Produkty getRodzaj() {
        return this.rodzaj;
    }

}
