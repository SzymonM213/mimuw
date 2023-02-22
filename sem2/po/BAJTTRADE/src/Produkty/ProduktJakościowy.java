package Produkty;

public class ProduktJakościowy extends Produkt {
    protected int poziom_jakości;

    public ProduktJakościowy(Produkt.Produkty rodzaj, int poziom_jakości) {
        super(rodzaj);
        this.poziom_jakości = poziom_jakości;
    }

    public int getPoziom_jakości() {
        return this.poziom_jakości;
    }
}
