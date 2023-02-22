package StrategieProdukcji;

import Giełdy.HistoriaGiełdy;
import Produkty.Produkt;

public class Średniak implements StrategiaProdukcji {

    private final int historia_średniej_produkcji;

    public Średniak(int historia_średniej_produkcji) {
        this.historia_średniej_produkcji = historia_średniej_produkcji;
    }

    @Override
    public Produkt.Produkty coProdukuje(HistoriaGiełdy historia, int[] produktywność) {
        int wynik = 0;
        double rekord = 0;
        double[] srednieCeny;
        int i;
        for (int j = 0; j < this.historia_średniej_produkcji; j++) {
            srednieCeny = historia.getSredniaCena(historia.getDzien() - 1 - j);
            for (i = 0; i < srednieCeny.length; i++) {
                if (srednieCeny[i] >= rekord) {
                    rekord = srednieCeny[i];
                    wynik = i;
                }
            }

        }
        return Produkt.Produkty.values()[wynik];
    }
}
