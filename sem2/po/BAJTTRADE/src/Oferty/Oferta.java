package Oferty;

import Produkty.Produkt;

public abstract class Oferta {
    private final int id_agenta;
    private final Produkt produkt;
    private int liczba;

    protected Oferta(int id_agenta, Produkt produkt, int liczba) {
        this.id_agenta = id_agenta;
        this.produkt = produkt;
        this.liczba = liczba;
    }

    public int getId_agenta() {
        return this.id_agenta;
    }

    public Produkt getProdukt() {
        return this.produkt;
    }

    public int getLiczba() {
        return this.liczba;
    }

    public void kup() {
        this.liczba--;
    }
}
