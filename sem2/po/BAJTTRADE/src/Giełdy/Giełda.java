package Giełdy;

import Agenci.Robotnik;
import Agenci.Spekulant;
import Oferty.OfertaRobotnika;
import Oferty.OfertaSpekulanta;
import Produkty.Produkt;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public abstract class Giełda {
    protected final Map<Integer,Robotnik> robotnicy;
    private final Map<Integer, Spekulant> spekulanci;
    private final HistoriaGiełdy historia;
    protected int dzień;

    protected Giełda(Map<Integer, Robotnik> robotnicy, Map<Integer, Spekulant> spekulanci) {
        this.robotnicy = new HashMap<>();
        for(var v: robotnicy.keySet()) {
            this.robotnicy.put(v, robotnicy.get(v));
        }
        this.spekulanci = new HashMap<>();
        for(var v: spekulanci.keySet()) {
            this.spekulanci.put(v, spekulanci.get(v));
        }
        this.historia = new HistoriaGiełdy();
        this.dzień = 0;
    }

    protected abstract List<Robotnik> sortujRobotników();

    private void przeprowadzDzien() {
        this.sortujRobotników();
        Map<Produkt.Produkty, List<OfertaRobotnika>> ofertyRobotników = new HashMap<>();
        //tablice do liczenia średniej ceny każdego produktu
        double[] sumaCen = new double[Produkt.Produkty.values().length];
        int[] ileSprzedano = new int[Produkt.Produkty.values().length];
        for(int i = 0;i<sumaCen.length;i++) {
            sumaCen[i] = 0;
            ileSprzedano[i] = 0;
        }
        for(var v: Produkt.Produkty.values()) {
            ofertyRobotników.put(v, new ArrayList<>());
        }
        //robotnicy wystawiają oferty
        List<Robotnik> listaRobotników = this.sortujRobotników();
        for(var v: listaRobotników) {
            if(v.getCzyŻyje()) {
                List<OfertaRobotnika> oferty_robotnika = robotnicy.get(v.id()).przeprowadźDzień(this.historia);
                if (oferty_robotnika.size() > 0) {
                    ofertyRobotników.get(oferty_robotnika.get(0).getProdukt().getRodzaj()).addAll(oferty_robotnika);
                    for (var o : oferty_robotnika) {
                        this.historia.dodajOfertę(o);
                    }
                }
            }
        }
        //spekulanci kupują
        for(var v: this.spekulanci.keySet()) {
            Map<Integer, List<OfertaSpekulanta>> transakcje = this.spekulanci.get(v).kupuj(ofertyRobotników, historia);
            for(var robotnik: transakcje.keySet()) {
                for(var transakcja: transakcje.get(robotnik)) {
                    sumaCen[transakcja.getProdukt().getRodzaj().ordinal()]+=transakcja.getCena();
                    ileSprzedano[transakcja.getProdukt().getRodzaj().ordinal()]++;
                    this.robotnicy.get(robotnik).zapłać(transakcja.getCena());
                }
            }
        }
        //spekulanci wystawiają oferty
        Map<Produkt.Produkty, List<OfertaSpekulanta>> ofertySpekulantów = new HashMap<>();
        for(var v: this.spekulanci.keySet()) {
            Map<Produkt.Produkty, List<OfertaSpekulanta>> ofertaSpekulanta = this.spekulanci.get(v).dajOferty(historia);
            for(var rodzaj: Produkt.Produkty.values()) {
                if (ofertySpekulantów.get(rodzaj) == null) ofertySpekulantów.put(rodzaj, new ArrayList<>());
                ofertySpekulantów.get(rodzaj).addAll(ofertaSpekulanta.get(rodzaj));
            }
        }
        for(var rodzaj: Produkt.Produkty.values()) {
            ofertySpekulantów.get(rodzaj).sort(OfertaSpekulanta::komparator);
        }
        //robotnicy kupują
        listaRobotników = this.sortujRobotników();
        for(var robotnik: listaRobotników) {
            if(robotnik.getCzyŻyje()) {
                for (var oferta : robotnik.kupuj(ofertySpekulantów)) {
                    this.spekulanci.get(oferta.getId_agenta()).zapłać(oferta.getCena());
                    sumaCen[oferta.getProdukt().getRodzaj().ordinal()] += oferta.getCena();
                    ileSprzedano[oferta.getProdukt().getRodzaj().ordinal()]++;
                }
            }
        }
        double[] średnieCeny = new double[sumaCen.length];
        for(int i = 0; i<średnieCeny.length;i++) {
            średnieCeny[i] = sumaCen[i]/ileSprzedano[i];
        }
        this.historia.dodajSrednieCeny(średnieCeny);
        this.historia.dodajOfertyRobotnika(ofertyRobotników);
        this.historia.dodajOfertySpekulanta(ofertySpekulantów);
        this.dzień++;
    }

    public void symuluj(int liczbaDni) {
        for(int i = 0;i<liczbaDni;i++) {
            this.przeprowadzDzien();
        }
    }
}
