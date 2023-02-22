package StrategieProdukcji;

import Giełdy.HistoriaGiełdy;
import Produkty.Produkt;

import java.util.Random;

public class Losowy implements StrategiaProdukcji {

    @Override
    public Produkt.Produkty coProdukuje(HistoriaGiełdy historia, int[] produktywność) {
        Random r = new Random();
        return Produkt.Produkty.values()[r.nextInt(Produkt.Produkty.values().length)];
    }

}
