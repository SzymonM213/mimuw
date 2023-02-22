package StrategieZmianyKariery;

import Giełdy.HistoriaGiełdy;
import ŚcieżkiKariery.ŚcieżkaKariery;

public interface StrategiaZmianyŚcieżki {
    ŚcieżkaKariery uczSię(ŚcieżkaKariery aktualnaKariera, int[] poziomyKarier, int n, HistoriaGiełdy historia);
}
