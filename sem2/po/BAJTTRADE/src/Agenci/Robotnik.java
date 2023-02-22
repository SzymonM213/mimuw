package Agenci;

import Giełdy.HistoriaGiełdy;
import Oferty.OfertaRobotnika;
import Oferty.OfertaSpekulanta;
import Produkty.*;
import StrategieDnia.StrategiaDnia;
import StrategieKupowania.StrategiaKupowania;
import StrategieProdukcji.StrategiaProdukcji;
import StrategieZmianyKariery.StrategiaZmianyŚcieżki;
import ŚcieżkiKariery.ŚcieżkaKariery;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static Produkty.Produkt.Produkty.*;
import static java.lang.Math.max;

public class Robotnik extends Agent{

    private boolean czyŻyje;
    private int dniBezJedzenia;
    private int[] poziomyKarier;
    private ŚcieżkaKariery ścieżkaKariery;
    private final StrategiaDnia strategiaDnia;
    private final StrategiaProdukcji strategiaProdukcji;
    private final StrategiaKupowania strategiaKupowania;
    private final StrategiaZmianyŚcieżki strategiaZmianyŚcieżki;
    private final int[] produktywność;
    private final int kara_za_brak_ubrań;
    protected int ileDziśWyprodukował;

    public Robotnik(int id, double diamenty, int jedzenie, Map<Produkt.Produkty, List<Produkt>> produkty, int[] poziomyKarier, ŚcieżkaKariery ścieżkaKariery,
                    StrategiaDnia strategiaDnia, StrategiaProdukcji strategiaProdukcji, StrategiaKupowania strategiaKupowania,
                    StrategiaZmianyŚcieżki strategiaZmianyŚcieżki, int[] produktywność, int kara_za_brak_ubrań) {
        super(id, diamenty, jedzenie, produkty);
        this.czyŻyje = true;
        this.ścieżkaKariery = ścieżkaKariery;
        this.strategiaDnia = strategiaDnia;
        this.strategiaProdukcji = strategiaProdukcji;
        this.strategiaKupowania = strategiaKupowania;
        this.strategiaZmianyŚcieżki = strategiaZmianyŚcieżki;
        this.produktywność = produktywność;
        this.kara_za_brak_ubrań = kara_za_brak_ubrań;
        this.poziomyKarier = poziomyKarier;
    }

    public double diamenty() {
        return this.diamenty;
    }

    public int id() {
        return this.id;
    }

    public static int komparatorKapitalistyczny(Robotnik o1, Robotnik o2) {
        if (o1.diamenty() > o2.diamenty) return 1;
        if (o1.diamenty() < o2.diamenty) return -1;
        if (o1.id() > o2.id) return 1;
        else return -1;
    }

    public static int komparatorSocjalistyczny(Robotnik o1, Robotnik o2) {
        return -komparatorKapitalistyczny(o1, o2);
    }

    //lista list bo to lista dni i każdy dzień ma listę ofert
    private void uczSię(HistoriaGiełdy historia) {
        int[] kopiaPoziomów = new int[this.poziomyKarier.length];
        for(int i = 0;i<this.poziomyKarier.length;i++) {
            kopiaPoziomów[i] = this.poziomyKarier[i];
        }
        this.ścieżkaKariery = this.strategiaZmianyŚcieżki.uczSię(this.ścieżkaKariery, kopiaPoziomów, max(1, this.id%17), historia);
        this.dniBezJedzenia = 0;
    }

    private int premiaPoziom(Produkt.Produkty produkt) {
        switch (this.poziomyKarier[produkt.getKariera().ordinal()]) {
            case 1 -> {
                return 50;
            }
            case 2 -> {
                return 150;
            }
            case 3 -> {
                return 300;
            }
            default -> {
                return 300+(produkt.getKariera().ordinal()-3)*25;
            }
        }
    }

    private int premiaJedzenie() {
        switch (this.dniBezJedzenia) {
            case 0 -> {
                return 0;
            }
            case 1 -> {
                return -100;
            }
            default -> {
                return -300;
            }
        }
    }

    private int premiaUbrania() {
        if(this.produkty.get(UBRANIA).size() < 100) {
            return -this.kara_za_brak_ubrań;
        }
        return 0;
    }

    private int premiaNarzędzia() {
        int wynik = 0;
        for(var n: this.produkty.get(NARZEDZIA)) {
            wynik += ((ProduktJakościowy) n).getPoziom_jakości();
        }
        return wynik;
    }

    public int premia(Produkt.Produkty produkt) {
        return this.premiaJedzenie()+this.premiaPoziom(produkt)+this.premiaUbrania()+this.premiaNarzędzia();
    }

    private void jedz() {
        if(this.jedzenie >= 100) {
            this.jedzenie -= 100;
            this.dniBezJedzenia = 0;
        }
        else {
            this.jedzenie = 0;
            this.dniBezJedzenia++;
        }
    }

    private void nośUbrania() {
        for(int i =0;i<this.produkty.get(UBRANIA).size() && i<100;i++) {
            ((Ubranie) this.produkty.get(UBRANIA).get(i)).noś();
            if(((ProduktJakościowy)this.produkty.get(UBRANIA).get(i)).getPoziom_jakości()==0) {
                this.produkty.get(UBRANIA).remove(i);
            }
        }
    }

    private Produkt.Produkty coProdukuje(HistoriaGiełdy historia) {
        int[] produktywność = new int[this.produktywność.length];
        for(int i = 0;i<produktywność.length;i++) {
            produktywność[i] = (this.produktywność[i]*this.premia(Produkt.Produkty.values()[i]))/100;
        }
        return this.strategiaProdukcji.coProdukuje(historia, produktywność);
    }

    private List<OfertaRobotnika> pracuj(HistoriaGiełdy historia) {
        List<OfertaRobotnika> oferty = new ArrayList<>();
        this.jedz();
        this.nośUbrania();

        Produkt.Produkty rodzaj = this.coProdukuje(historia);
        int ile_produkuje = this.produktywność[rodzaj.ordinal()]*(100+this.premia(rodzaj))/100;
        this.ileDziśWyprodukował = ile_produkuje;
        if(rodzaj == DIAMENTY) this.diamenty += ile_produkuje;
        else {
            Map<Integer, Integer> poziomy_produktów = this.strategiaKupowania.poziomy_produktów(rodzaj, ile_produkuje, this.produkty.get(PROGRAMY_KOMPUTEROWE));
            for(int i=0; i<poziomy_produktów.values().size();i++) {
                oferty.add(new OfertaRobotnika(this.id, Produkt.produkt(rodzaj, i), poziomy_produktów.get(i)));
            }
        }
        return oferty;
    }

    public List<OfertaRobotnika> przeprowadźDzień(HistoriaGiełdy historia) {
        if(this.dniBezJedzenia == 3) {
            this.czyŻyje = false;
            this.diamenty = 0;
            return new ArrayList<>();
        }
        if(this.strategiaDnia.czyPracuje(this.diamenty, historia)) {
            return this.pracuj(historia);
        }
        else {
            this.uczSię(historia);
            return new ArrayList<>();
        }
    }

    public List<OfertaSpekulanta> kupuj(Map<Produkt.Produkty, List<OfertaSpekulanta>> ofertySpekulantów) {
        Double noweDiamenty = this.diamenty;
        List<OfertaSpekulanta> wynik = this.strategiaKupowania.kupuj(ofertySpekulantów, noweDiamenty,
                this.produkty.get(UBRANIA).size(), this.ileDziśWyprodukował);
        this.diamenty = noweDiamenty;
        return wynik;
    }

    public boolean getCzyŻyje() {
        return this.czyŻyje;
    }
}
