package Produkty;

public class Ubranie extends ProduktJakościowy {

    public Ubranie(int poziom_jakości) {
        super(Produkty.UBRANIA, poziom_jakości);
    }

    public void noś() {
        this.poziom_jakości--;
    }
}
