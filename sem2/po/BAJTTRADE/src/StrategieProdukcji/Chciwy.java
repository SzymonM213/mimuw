package StrategieProdukcji;

import Giełdy.HistoriaGiełdy;
import Produkty.Produkt;

public class Chciwy implements StrategiaProdukcji {

    @Override
    public Produkt.Produkty coProdukuje(HistoriaGiełdy historia, int[] produktywność) {
        int wynik=0;
        double rekord=0;
        double[] srednieCeny = historia.getSredniaCena(historia.getDzien()-1);
        for(int i = 0; i< srednieCeny.length; i++) {
            if(srednieCeny[i]*produktywność[i] >= rekord) {
                rekord = srednieCeny[i]*produktywność[i];
                wynik = i;
            }
        }
        return Produkt.Produkty.values()[wynik];
    }
}
