package Giełdy;

import Agenci.Robotnik;
import Agenci.Spekulant;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class Socjalistyczna extends Giełda {

    public Socjalistyczna(Map<Integer, Robotnik> robotnicy, Map<Integer, Spekulant> spekulanci) {
        super(robotnicy, spekulanci);
    }

    @Override
    protected List<Robotnik> sortujRobotników() {
        List<Robotnik> wynik = new ArrayList<>();
        for(var v: this.robotnicy.values()) wynik.add(v);
        wynik.sort(Robotnik::komparatorSocjalistyczny);
        return wynik;
    }
}
