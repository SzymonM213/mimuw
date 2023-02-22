package Agenci;

import Produkty.Produkt;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public abstract class Agent {
    protected int id;
    protected double diamenty;
    protected int jedzenie;
    protected Map<Produkt.Produkty, List<Produkt>> produkty;

    public Agent(int id, double diamenty, int jedzenie, Map<Produkt.Produkty, List<Produkt>> produkty) {
        this.id = id;
        this.diamenty = diamenty;
        this.jedzenie = jedzenie;
        this.produkty = new HashMap<>();
        for(var v: produkty.keySet()) {
            List<Produkt> kopia = new ArrayList<>();
            for(var w: produkty.get(v)) {
                kopia.add(w);
            }
            this.produkty.put(v ,kopia);
        }
    }

    public int id() {
        return this.id;
    }

    public void zapłać(double cena) {
        this.diamenty += cena;
    }
}
