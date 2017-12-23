/**
 * Created by shubhimittal on 3/25/17.
 */


import java.util.LinkedList;
import java.util.List;

class BenchMarkResult {

    private String ap1;
    private String ap2;
    private int time;

    BenchMarkResult(String ap1, String ap2, int time) {
        this.ap1 = ap1;
        this.ap2 = ap2;
        this.time = time;
    }
}

public class Benchmark {

    private List<Flight> flights = new LinkedList<>();
    private List<String> airports = new LinkedList<>();
    private RacketList<Flight> flightRacketList = RacketLists.empty();


    Benchmark(int n) {

        this.flights = makeStressTest0(n, this.flights);
        getAirports();
        this.flightRacketList = convertToRacketList(flights);

    }

    public List<Flight> makeStressTest0 (int n, List<Flight> flights) {

        if (n == 0) {
            return flights;
        } else {
            String name = "NoWays ";
            String flightName1 = name.concat(Integer.toString((n + n)));
            String flightName2 = name.concat(Integer.toString((n + n + 1)));
            String apName1 = "AP ".concat(Integer.toString(n));
            String apName2 = "AP ".concat(Integer.toString((n + 1)));
            UTC t1 = UTCs.make(((107 * n) % 24), ((223 * n) % 60));
            UTC t2 = UTCs.make(((151 * n) % 24), ((197 * n) % 60));
            UTC t3 = UTCs.make(((163 * n) % 24), ((201 * n) % 60));
            UTC t4 = UTCs.make(((295 * n) % 24), ((183 * n) % 60));
            Flight f1 = Flights.make(flightName1, apName1, apName2, t1, t2);
            Flight f2 = Flights.make(flightName2, apName1, apName2, t3, t4);

            //flights = flights.cons(f1).cons(f2);
            flights.add(f1);
            flights.add(f2);

            return makeStressTest0(n - 1, flights);
        }
    }

    private void getAirports() {
        //TODO: improve code, use sets to reduce iterations
        List<String> temp = new LinkedList<String>();
        for (Flight f : flights) {
            temp.add(f.departs());
            temp.add(f.arrives());
        }
        for (String airportName: temp) {
            if(!(airports.contains(airportName))) {
                airports.add(airportName);
            }
        }
    }


    private RacketList<Flight> convertToRacketList(List<Flight> flights) {

        RacketList<Flight> flightrls = RacketLists.empty();
        for (int i=flights.size()-1; i>=0; i--) {
            flightrls = flightrls.cons(flights.get(i));
        }
        return flightrls;
    }


    public List<BenchMarkResult> benchmark0() {

        List<BenchMarkResult> b = new LinkedList<>();

        for (String ap1: airports) {

            for(String ap2: airports){
                if (Schedules.canGetThere(ap1, ap2, flightRacketList)) {
                    b.add(new BenchMarkResult(
                            ap1, ap2, Schedules.travelTime(
                                    ap1, ap2, flightRacketList)));
                } else {
                    b.add(new BenchMarkResult(ap1, ap2, -1));
                }
            }
        }
        return b;
    }


    public static void eachIteration(int n) {

        final long startTime = System.currentTimeMillis();
        Benchmark b = new Benchmark(n);
        List<BenchMarkResult> blist = new LinkedList<>();
        blist = b.benchmark0();
        final long duration = System.currentTimeMillis() - startTime;
        System.out.println(
                Integer.toString(n) + "      " + Long.toString(duration));

    }

    public static void main(String[] args) {
        for (int i=20; i<=100; i=i+2) {
            eachIteration(i);
        }
    }

}
