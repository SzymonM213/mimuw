package StrategieProdukcji;

import Giełdy.HistoriaGiełdy;
import Produkty.Produkt;

public interface StrategiaProdukcji {

    Produkt.Produkty coProdukuje(HistoriaGiełdy historia, int[] produktywność);
}
