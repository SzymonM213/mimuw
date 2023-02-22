package StrategieProdukcji;

import Giełdy.HistoriaGiełdy;
import Produkty.Produkt;

public class Perspektywiczny implements StrategiaProdukcji {

    private final int historia_perspektywy;

    public Perspektywiczny(int historia_perspektywy) {
        this.historia_perspektywy = historia_perspektywy;
    }

    @Override
    public Produkt.Produkty coProdukuje(HistoriaGiełdy historia, int[] produktywność) {
        int wynik=0;
        double rekord=0;
        double[] srednieCeny = historia.getSredniaCena(historia.getDzien()-1);
        double[] srednieCenyKiedys = historia.getSredniaCena(historia.getDzien()-this.historia_perspektywy-1);
        for(int i = 0; i< srednieCeny.length; i++) {
            if(srednieCeny[i]-srednieCenyKiedys[i] >= rekord) {
                rekord = srednieCeny[i]-srednieCenyKiedys[i];
                wynik = i;
            }
        }
        return Produkt.Produkty.values()[wynik];
    }
}
