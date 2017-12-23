import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.function.BiFunction;

public class PdpQ2Tests {

    // Tests based on flights used to test Problem Set 00.

    // Abbreviations for calls to static methods defined in Schedules.

    static boolean canGetThere (String ap1,
            String ap2,
            RacketList<Flight> flights) {
        return Schedules.canGetThere (ap1, ap2, flights);
    }

    static RacketList<Flight> fastestItinerary (String ap1,
            String ap2,
            RacketList<Flight> flights) {
        return Schedules.fastestItinerary (ap1, ap2, flights);
    }

    static int travelTime (String ap1,
            String ap2,
            RacketList<Flight> flights) {
        return Schedules.travelTime (ap1, ap2, flights);
    }

    // RETURNS: true iff the given lists of flights are equal.

    static Boolean isEqual (RacketList<Flight> flights1,
            RacketList<Flight> flights2) {
        if (flights1.isEmpty())
            return flights2.isEmpty();
        else if (flights2.isEmpty())
            return false;
        else if (flights1.first().isEqual(flights2.first()))
            return isEqual (flights1.rest(), flights2.rest());
        else
            return false;
    }

    // RETURNS: a readable representation of the flight as a string

    static String flightToString (Flight f) {
        UTC t1 = f.departsAt();
        UTC t2 = f.arrivesAt();
        int h1 = t1.hour();
        int h2 = t2.hour();
        int m1 = t1.minute();
        int m2 = t2.minute();
        int utc1 = 100 * h1 + m1;
        int utc2 = 100 * h2 + m2;
        String result = f.name() + " " + f.departs() + " " + f.arrives();
        return result + " " + utc1 + " " + utc2;
    }

    static Flight makeFlight (String name, String ap1, String ap2,
            int lv, int ar) {
        UTC t1 = UTCs.make (lv / 100, lv % 100);
        UTC t2 = UTCs.make (ar / 100, ar % 100);
        return Flights.make (name, ap1, ap2, t1, t2);
    }

    // Pan American is no longer flying.

    static RacketList<Flight> panAmFlights = RacketLists.empty();

    // From 1 December 2016 to 15 January 2017,
    // Delta's Worldwide Timetable is 1562 pages long (53.5 megabytes).
    //
    // We'll use only a small number of those flights for testing.

    static RacketList<Flight> deltaFlights = makeDeltaFlights();

    static RacketList<Flight> makeDeltaFlights () {
        List<Flight> flights = new ArrayList<Flight>();

        flights.add (makeFlight ("Delta 0121", "LGA", "MSP", 1100, 1409));
        flights.add (makeFlight ("Delta 2163", "MSP", "PDX", 1500, 1902));
        flights.add (makeFlight ("Delta 2079", "BOS", "DTW", 1035, 1259));
        flights.add (makeFlight ("Delta 1523", "BOS", "DTW", 2158,   20));
        flights.add (makeFlight ("Delta 0058", "BOS", "LHR",   44,  720));
        flights.add (makeFlight ("Delta 2531", "BOS", "LAX", 1317, 2020));
        flights.add (makeFlight ("Delta 2532", "BOS", "LAX", 2250,  555));
        flights.add (makeFlight ("Delta 1959", "BOS", "MSP", 1050, 1417));
        flights.add (makeFlight ("Delta 1894", "BOS", "MSP", 1355, 1730));
        flights.add (makeFlight ("Delta 2391", "BOS", "MSP", 2135,  105));
        flights.add (makeFlight ("Delta 2734", "BOS", "LGA", 1100, 1230));
        flights.add (makeFlight ("Delta 3550", "BZN", "LAX", 2020, 2302));
        flights.add (makeFlight ("Delta 1601", "DEN", "DTW", 1305, 1611));
        flights.add (makeFlight ("Delta 0916", "DEN", "DTW", 2332,  219));
        flights.add (makeFlight ("Delta 0010", "DEN", "LHR", 2030,  945));
        flights.add (makeFlight ("Delta 5703", "DEN", "LAX", 1404, 1715));
        flights.add (makeFlight ("Delta 5743", "DEN", "LAX",   34,  331));
        flights.add (makeFlight ("Delta 2437", "DTW", "BOS", 1345, 1546));
        flights.add (makeFlight ("Delta 0158", "DTW", "BOS", 1700, 1855));
        flights.add (makeFlight ("Delta 1700", "DTW", "BOS", 2240,  042));
        flights.add (makeFlight ("Delta 1511", "DTW", "DEN", 1330, 1651));
        flights.add (makeFlight ("Delta 1645", "DTW", "DEN", 1711, 2038));
        flights.add (makeFlight ("Delta 1706", "DTW", "LAX", 1320, 1845));
        flights.add (makeFlight ("Delta 0249", "DTW", "MSP", 1500, 1707));
        flights.add (makeFlight ("Delta 2359", "DTW", "MSP", 1715, 1920));
        flights.add (makeFlight ("Delta 2476", "DTW", "MSP",  110,  318));
        flights.add (makeFlight ("Delta 0059", "LHR", "BOS",  920, 1726));
        flights.add (makeFlight ("Delta 4378", "LHR", "BOS", 1645,   20));
        flights.add (makeFlight ("Delta 0011", "LHR", "DEN", 1255,  220));
        flights.add (makeFlight ("Delta 0302", "LAX", "BOS", 1625, 2214));
        flights.add (makeFlight ("Delta 5732", "LAX", "BZN",   30,  318));
        flights.add (makeFlight ("Delta 4574", "LAX", "DEN", 1735, 2007));
        flights.add (makeFlight ("Delta 5700", "LAX", "DEN",   10,  245));
        flights.add (makeFlight ("Delta 2077", "LAX", "PDX", 1735, 2009));
        flights.add (makeFlight ("Delta 1728", "MSP", "BOS", 1600, 1851));
        flights.add (makeFlight ("Delta 2305", "MSP", "BZN",  221,  513));
        flights.add (makeFlight ("Delta 1609", "MSP", "DEN", 2035, 2252));
        flights.add (makeFlight ("Delta 1836", "MSP", "DTW", 1224, 1415));
        flights.add (makeFlight ("Delta 1734", "MSP", "DTW", 1755, 1941));
        flights.add (makeFlight ("Delta 0592", "MSP", "LGA", 1730, 2017));
        flights.add (makeFlight ("Delta 2734", "LGA", "BOS", 1100, 1208));
        flights.add (makeFlight ("Delta 1294", "LGA", "DEN", 1310, 1754));
        flights.add (makeFlight ("Delta 0879", "LGA", "DTW", 1410, 1620));
        flights.add (makeFlight ("Delta 1422", "LGA", "MSP", 1500, 1822));
        flights.add (makeFlight ("Delta 0950", "PDX", "LAX", 1418, 1655));
        flights.add (makeFlight ("Delta 2077", "PDX", "LAX", 2045, 2314));
        flights.add (makeFlight ("Delta 2831", "PDX", "LAX", 2346,  225));
        flights.add (makeFlight ("Delta 2167", "PDX", "MSP", 2200,  120));

        return asRacketList (flights);
    }

    // A long cycle of Delta flights that has only "BOS" in common
    // with the airports served by the deltaFlights example above.

    static RacketList<Flight> deltaCycle = makeDeltaCycle();

    static RacketList<Flight> makeDeltaCycle () {
        List<Flight> flights = new ArrayList<Flight>();

        flights.add (makeFlight ("Delta 0105", "BOS", "ATL", 1950, 2259));
        flights.add (makeFlight ("Delta 1895", "ATL", "PHL", 1505, 1705));
        flights.add (makeFlight ("Delta 0926", "PHL", "SLC", 1059, 1615));
        flights.add (makeFlight ("Delta 5828", "SLC", "DFW", 1813, 2056));
        flights.add (makeFlight ("Delta 8122", "DFW", "MEX",  132,  435));
        flights.add (makeFlight ("Delta 8028", "MEX", "LAS", 1800, 2228));
        flights.add (makeFlight ("Delta 2837", "LAS", "MKC",  215,  505));
        flights.add (makeFlight ("Delta 3337", "MKC", "ORL", 2000, 2250));
        flights.add (makeFlight ("Delta 3617", "ORL", "BNA", 1735, 1936));
        flights.add (makeFlight ("Delta 4811", "BNA", "CVG", 1215, 1333));
        flights.add (makeFlight ("Delta 6207", "CVG", "IAH", 1850, 2131));
        flights.add (makeFlight ("Delta 0108", "IAH", "MAD", 2006,  715));
        flights.add (makeFlight ("Delta 6775", "MAD", "MIA", 1425, 2350));
        flights.add (makeFlight ("Delta 7199", "MIA", "YTO", 2055,    6));
        flights.add (makeFlight ("Delta 7037", "YTO", "BOS", 2215,    5));

        return asRacketList (flights);
    }

    static RacketList<Flight> shortCycle = makeShortCycle();

    static RacketList<Flight> makeShortCycle () {
        List<Flight> flights = new ArrayList<Flight>();

        flights.add (makeFlight ("Delta 0105", "BOS", "ATL", 1950, 2259));
        flights.add (makeFlight ("Delta 1895", "ATL", "PHL", 1505, 1705));
        flights.add (makeFlight ("Delta 0926", "PHL", "SLC", 1059, 1615));
        flights.add (makeFlight ("Delta 7037", "SLC", "BOS", 2215,    5));

        return asRacketList (flights);
    }

    // Returns a list of all airports served by a flight in the given list.

    static List<String> allAirports (RacketList<Flight> flights) {
        List<String> result = new ArrayList<String>();
        while (! (flights.isEmpty())) {
            Flight f = flights.first();
            String ap1 = f.departs();
            String ap2 = f.arrives();
            if (! (result.contains (ap1)))
                result.add (ap1);
            if (! (result.contains (ap2)))
                result.add (ap2);
            flights = flights.rest();
        }
        Collections.sort (result);
        return result;
    }

    // Is it possible to get from every airport to every other airport?

    static boolean isStronglyConnected (RacketList<Flight> flights) {
        List<String> airports = allAirports (flights);
        for (String ap1 : airports)
            for (String ap2 : airports)
                if (! (canGetThere (ap1, ap2, flights)))
                    return false;
        return true;
    }

    // Visit all pairs of things from the lists.

    static <T,U,R> List<R> visitAllPairs (BiFunction<T,U,R> f,
            List<T> things1,
            List<U> things2) {
        List<R> result = new ArrayList<R>();
        for (T x : things1) {
            for (U y : things2) {
                result.add (f.apply (x, y));
            }
        }
        return result;
    }

    // Visit all pairs of airports.

    static List<String> visitAllPairs (BiFunction<String,String,String> f,
            RacketList<Flight> flights) {
        List<String> airports = allAirports (flights);
        return visitAllPairs (f, airports, airports);
    }

    // Returns a list of strings naming a pair of airports
    // and giving the travel time between them.

    static List<String> allTravelTimes (RacketList<Flight> flights) {
        BiFunction<String,String,String> f
        = (String ap1, String ap2)
        -> (ap1 + " " + ap2 + " " + travelTime (ap1, ap2, flights));
        return visitAllPairs (f, flights);
    }

    static <E> RacketList<E> list (E x1) {
        RacketList<E> result = RacketLists.empty();
        result = result.cons (x1);
        return result;
    }

    static <E> RacketList<E> list (E x1, E x2) {
        RacketList<E> result = RacketLists.empty();
        result = result.cons (x2);
        result = result.cons (x1);
        return result;
    }

    static <E> RacketList<E> list (E x1, E x2, E x3) {
        RacketList<E> result = RacketLists.empty();
        result = result.cons (x3);
        result = result.cons (x2);
        result = result.cons (x1);
        return result;
    }

    static <E> RacketList<E> list (E x1, E x2, E x3, E x4) {
        RacketList<E> result = RacketLists.empty();
        result = result.cons (x4);
        result = result.cons (x3);
        result = result.cons (x2);
        result = result.cons (x1);
        return result;
    }

    // GIVEN: a non-negative integer n
    // RETURNS: a list of n(n-1) flights connecting n airports
    //     (complete graph on n airports)

    static RacketList<Flight> makeStressTest2 (int n) {
        if (n == 0)
            return RacketLists.empty();
        List<String> airports = makeAirports (n);
        List<Flight> result = new ArrayList<Flight>();
        for (String ap1 : airports) {
            for (String ap2 : airports) {
                if (! (ap1.equals(ap2))) {
                    result.add (makeRandomFlight (ap1, ap2));
                }
            }
        }
        return asRacketList (result);
    }

    static List<String> makeAirports (int n) {
        List<String> result = new ArrayList<String>();
        for (int i = 0; i < n; i = i + 1) {
            result.add ("AP" + (i + 100));
        }
        return result;
    }

    static Random rng = new Random (3276783576246906946L);

    static Flight makeRandomFlight (String ap1, String ap2) {
        return Flights.make (ap1+ap2,
                ap1,
                ap2,
                randomUTC(),
                randomUTC());
    }

    static UTC randomUTC () {
        int h = rng.nextInt (24);
        int m = rng.nextInt (60);
        return UTCs.make (h, m);
    }

    // Returns a RacketList with the same elements as the given list.

    static <E> RacketList<E> asRacketList (List<E> lst0) {
        RacketList<E> rev = RacketLists.empty();
        for (E x : lst0) {
            rev = rev.cons (x);
        }
        return reverse (rev);
    }

    // Returns a representation of the given list in reverse order.

    static <E> RacketList<E> reverse (RacketList<E> lst) {
        RacketList<E> result = RacketLists.empty();
        while (! (lst.isEmpty())) {
            result = result.cons (lst.first());
            lst = lst.rest();
        }
        return result;
    }

    static void benchmark2 (int n) {
        RacketList<Flight> flights = makeStressTest2 (n);
        if (n > 1) {
            Flight f1 = flights.first();
            Flight f2 = flights.rest().first();
            String ap1 = f1.departs();
            String ap2 = f2.arrives();
            long t0 = System.currentTimeMillis();
            Schedules.travelTime (ap1, ap2, flights);
            long t1 = System.currentTimeMillis();
            long dt = t1 - t0;
            System.out.println ("benchmark2(" + n + "): " + dt + "ms");
        }
    }

    public static void main(String[] args) {
        PdpTestSuite tests = new PdpTestSuite(30);

        tests.addTestCase("you're already there!",
                () -> canGetThere ("ROT", "ROT", panAmFlights),  true);

        tests.addTestCase("ROT isn't served by Delta",
                () -> canGetThere ("ROT", "BOS", panAmFlights),  false);

        tests.addTestCase("TEN isn't served by Delta",
                () -> canGetThere ("BOS", "TEN", panAmFlights),  false);
        
        tests.addTestCase("the empty graph is strongly connected",
                () -> isStronglyConnected (panAmFlights), true);
        
        tests.addTestCase("deltaFlights is strongly connected",
                () -> isStronglyConnected (deltaFlights), true);
        
        tests.addTestCase("deltaCycle is strongly connected",
                () -> isStronglyConnected (deltaCycle), true);
        
        tests.addTestCase("deltaCycle is not strongly connected without BOS",
                () -> isStronglyConnected (deltaCycle.rest()), false);

        tests.addTestCase("itinerary for DEN to DEN via Pan Am",
                () -> fastestItinerary ("DEN", "DEN", panAmFlights).isEmpty(), true);

        tests.addTestCase("itinerary for BAL to BAL via Delta",
                () -> fastestItinerary ("BAL", "BAL", panAmFlights).isEmpty(), true);
        
        tests.addTestCase("itinerary for DEN to DTW via Delta",
                () -> fastestItinerary ("DEN", "DTW", deltaFlights),
                list (makeFlight ("Delta 0916", "DEN", "DTW", 2332, 219)),
                PdpQ2Tests::isEqual);
        
        tests.addTestCase("travel time for BOS to DTW",
                () -> travelTime ("BOS", "DTW", deltaFlights), 142);
        
        tests.addTestCase("itinerary for PDX to LHR via Delta",
                () -> fastestItinerary ("PDX", "LHR", deltaFlights),
                list (makeFlight ("Delta 0950", "PDX", "LAX",
                        1418, 1655),
                    makeFlight ("Delta 4574", "LAX", "DEN",
                        1735, 2007),
                    makeFlight ("Delta 0010", "DEN", "LHR",
                        2030, 945)),
                PdpQ2Tests::isEqual);
        
        tests.addTestCase("travel time for BZN to LGA",
                () -> travelTime ("BZN", "LGA", deltaFlights),
                2410);
        
        tests.addTestCase("travel time for BZN to LHR",
                () -> travelTime ("BZN", "LHR", deltaFlights),
                2100);

        List<String> expected = new ArrayList<String>();
        expected.add ("ATL" + " " + "ATL" + " " + 0);
        expected.add ("ATL" + " " + "BNA" + " " + 6031);
        expected.add ("ATL" + " " + "BOS" + " " + 13500);
        expected.add ("ATL" + " " + "CVG" + " " + 7108);
        expected.add ("ATL" + " " + "DFW" + " " + 1791);
        expected.add ("ATL" + " " + "IAH" + " " + 7586);
        expected.add ("ATL" + " " + "LAS" + " " + 3323);
        expected.add ("ATL" + " " + "MAD" + " " + 9610);
        expected.add ("ATL" + " " + "MEX" + " " + 2250);
        expected.add ("ATL" + " " + "MIA" + " " + 10605);
        expected.add ("ATL" + " " + "MKC" + " " + 3720);
        expected.add ("ATL" + " " + "ORL" + " " + 4785);
        expected.add ("ATL" + " " + "PHL" + " " + 120);
        expected.add ("ATL" + " " + "SLC" + " " + 1510);
        expected.add ("ATL" + " " + "YTO" + " " + 12061);
        expected.add ("BNA" + " " + "ATL" + " " + 7844);
        expected.add ("BNA" + " " + "BNA" + " " + 0);
        expected.add ("BNA" + " " + "BOS" + " " + 6470);
        expected.add ("BNA" + " " + "CVG" + " " + 78);
        expected.add ("BNA" + " " + "DFW" + " " + 10601);
        expected.add ("BNA" + " " + "IAH" + " " + 556);
        expected.add ("BNA" + " " + "LAS" + " " + 12133);
        expected.add ("BNA" + " " + "MAD" + " " + 2580);
        expected.add ("BNA" + " " + "MEX" + " " + 11060);
        expected.add ("BNA" + " " + "MIA" + " " + 3575);
        expected.add ("BNA" + " " + "MKC" + " " + 12530);
        expected.add ("BNA" + " " + "ORL" + " " + 13595);
        expected.add ("BNA" + " " + "PHL" + " " + 8930);
        expected.add ("BNA" + " " + "SLC" + " " + 10320);
        expected.add ("BNA" + " " + "YTO" + " " + 5031);
        expected.add ("BOS" + " " + "ATL" + " " + 189);
        expected.add ("BOS" + " " + "BNA" + " " + 7186);
        expected.add ("BOS" + " " + "BOS" + " " + 0);
        expected.add ("BOS" + " " + "CVG" + " " + 8263);
        expected.add ("BOS" + " " + "DFW" + " " + 2946);
        expected.add ("BOS" + " " + "IAH" + " " + 8741);
        expected.add ("BOS" + " " + "LAS" + " " + 4478);
        expected.add ("BOS" + " " + "MAD" + " " + 10765);
        expected.add ("BOS" + " " + "MEX" + " " + 3405);
        expected.add ("BOS" + " " + "MIA" + " " + 11760);
        expected.add ("BOS" + " " + "MKC" + " " + 4875);
        expected.add ("BOS" + " " + "ORL" + " " + 5940);
        expected.add ("BOS" + " " + "PHL" + " " + 1275);
        expected.add ("BOS" + " " + "SLC" + " " + 2665);
        expected.add ("BOS" + " " + "YTO" + " " + 13216);
        expected.add ("CVG" + " " + "ATL" + " " + 7449);
        expected.add ("CVG" + " " + "BNA" + " " + 14446);
        expected.add ("CVG" + " " + "BOS" + " " + 6075);
        expected.add ("CVG" + " " + "CVG" + " " + 0);
        expected.add ("CVG" + " " + "DFW" + " " + 10206);
        expected.add ("CVG" + " " + "IAH" + " " + 161);
        expected.add ("CVG" + " " + "LAS" + " " + 11738);
        expected.add ("CVG" + " " + "MAD" + " " + 2185);
        expected.add ("CVG" + " " + "MEX" + " " + 10665);
        expected.add ("CVG" + " " + "MIA" + " " + 3180);
        expected.add ("CVG" + " " + "MKC" + " " + 12135);
        expected.add ("CVG" + " " + "ORL" + " " + 13200);
        expected.add ("CVG" + " " + "PHL" + " " + 8535);
        expected.add ("CVG" + " " + "SLC" + " " + 9925);
        expected.add ("CVG" + " " + "YTO" + " " + 4636);
        expected.add ("DFW" + " " + "ATL" + " " + 12807);
        expected.add ("DFW" + " " + "BNA" + " " + 3964);
        expected.add ("DFW" + " " + "BOS" + " " + 11433);
        expected.add ("DFW" + " " + "CVG" + " " + 5041);
        expected.add ("DFW" + " " + "DFW" + " " + 0);
        expected.add ("DFW" + " " + "IAH" + " " + 5519);
        expected.add ("DFW" + " " + "LAS" + " " + 1256);
        expected.add ("DFW" + " " + "MAD" + " " + 7543);
        expected.add ("DFW" + " " + "MEX" + " " + 183);
        expected.add ("DFW" + " " + "MIA" + " " + 8538);
        expected.add ("DFW" + " " + "MKC" + " " + 1653);
        expected.add ("DFW" + " " + "ORL" + " " + 2718);
        expected.add ("DFW" + " " + "PHL" + " " + 13893);
        expected.add ("DFW" + " " + "SLC" + " " + 15283);
        expected.add ("DFW" + " " + "YTO" + " " + 9994);
        expected.add ("IAH" + " " + "ATL" + " " + 5933);
        expected.add ("IAH" + " " + "BNA" + " " + 12930);
        expected.add ("IAH" + " " + "BOS" + " " + 4559);
        expected.add ("IAH" + " " + "CVG" + " " + 14007);
        expected.add ("IAH" + " " + "DFW" + " " + 8690);
        expected.add ("IAH" + " " + "IAH" + " " + 0);
        expected.add ("IAH" + " " + "LAS" + " " + 10222);
        expected.add ("IAH" + " " + "MAD" + " " + 669);
        expected.add ("IAH" + " " + "MEX" + " " + 9149);
        expected.add ("IAH" + " " + "MIA" + " " + 1664);
        expected.add ("IAH" + " " + "MKC" + " " + 10619);
        expected.add ("IAH" + " " + "ORL" + " " + 11684);
        expected.add ("IAH" + " " + "PHL" + " " + 7019);
        expected.add ("IAH" + " " + "SLC" + " " + 8409);
        expected.add ("IAH" + " " + "YTO" + " " + 3120);
        expected.add ("LAS" + " " + "ATL" + " " + 11324);
        expected.add ("LAS" + " " + "BNA" + " " + 2481);
        expected.add ("LAS" + " " + "BOS" + " " + 9950);
        expected.add ("LAS" + " " + "CVG" + " " + 3558);
        expected.add ("LAS" + " " + "DFW" + " " + 14081);
        expected.add ("LAS" + " " + "IAH" + " " + 4036);
        expected.add ("LAS" + " " + "LAS" + " " + 0);
        expected.add ("LAS" + " " + "MAD" + " " + 6060);
        expected.add ("LAS" + " " + "MEX" + " " + 14540);
        expected.add ("LAS" + " " + "MIA" + " " + 7055);
        expected.add ("LAS" + " " + "MKC" + " " + 170);
        expected.add ("LAS" + " " + "ORL" + " " + 1235);
        expected.add ("LAS" + " " + "PHL" + " " + 12410);
        expected.add ("LAS" + " " + "SLC" + " " + 13800);
        expected.add ("LAS" + " " + "YTO" + " " + 8511);
        expected.add ("MAD" + " " + "ATL" + " " + 4834);
        expected.add ("MAD" + " " + "BNA" + " " + 11831);
        expected.add ("MAD" + " " + "BOS" + " " + 3460);
        expected.add ("MAD" + " " + "CVG" + " " + 12908);
        expected.add ("MAD" + " " + "DFW" + " " + 7591);
        expected.add ("MAD" + " " + "IAH" + " " + 13386);
        expected.add ("MAD" + " " + "LAS" + " " + 9123);
        expected.add ("MAD" + " " + "MAD" + " " + 0);
        expected.add ("MAD" + " " + "MEX" + " " + 8050);
        expected.add ("MAD" + " " + "MIA" + " " + 565);
        expected.add ("MAD" + " " + "MKC" + " " + 9520);
        expected.add ("MAD" + " " + "ORL" + " " + 10585);
        expected.add ("MAD" + " " + "PHL" + " " + 5920);
        expected.add ("MAD" + " " + "SLC" + " " + 7310);
        expected.add ("MAD" + " " + "YTO" + " " + 2021);
        expected.add ("MEX" + " " + "ATL" + " " + 11819);
        expected.add ("MEX" + " " + "BNA" + " " + 2976);
        expected.add ("MEX" + " " + "BOS" + " " + 10445);
        expected.add ("MEX" + " " + "CVG" + " " + 4053);
        expected.add ("MEX" + " " + "DFW" + " " + 14576);
        expected.add ("MEX" + " " + "IAH" + " " + 4531);
        expected.add ("MEX" + " " + "LAS" + " " + 268);
        expected.add ("MEX" + " " + "MAD" + " " + 6555);
        expected.add ("MEX" + " " + "MEX" + " " + 0);
        expected.add ("MEX" + " " + "MIA" + " " + 7550);
        expected.add ("MEX" + " " + "MKC" + " " + 665);
        expected.add ("MEX" + " " + "ORL" + " " + 1730);
        expected.add ("MEX" + " " + "PHL" + " " + 12905);
        expected.add ("MEX" + " " + "SLC" + " " + 14295);
        expected.add ("MEX" + " " + "YTO" + " " + 9006);
        expected.add ("MIA" + " " + "ATL" + " " + 3004);
        expected.add ("MIA" + " " + "BNA" + " " + 10001);
        expected.add ("MIA" + " " + "BOS" + " " + 1630);
        expected.add ("MIA" + " " + "CVG" + " " + 11078);
        expected.add ("MIA" + " " + "DFW" + " " + 5761);
        expected.add ("MIA" + " " + "IAH" + " " + 11556);
        expected.add ("MIA" + " " + "LAS" + " " + 7293);
        expected.add ("MIA" + " " + "MAD" + " " + 13580);
        expected.add ("MIA" + " " + "MEX" + " " + 6220);
        expected.add ("MIA" + " " + "MIA" + " " + 0);
        expected.add ("MIA" + " " + "MKC" + " " + 7690);
        expected.add ("MIA" + " " + "ORL" + " " + 8755);
        expected.add ("MIA" + " " + "PHL" + " " + 4090);
        expected.add ("MIA" + " " + "SLC" + " " + 5480);
        expected.add ("MIA" + " " + "YTO" + " " + 191);
        expected.add ("MKC" + " " + "ATL" + " " + 10259);
        expected.add ("MKC" + " " + "BNA" + " " + 1416);
        expected.add ("MKC" + " " + "BOS" + " " + 8885);
        expected.add ("MKC" + " " + "CVG" + " " + 2493);
        expected.add ("MKC" + " " + "DFW" + " " + 13016);
        expected.add ("MKC" + " " + "IAH" + " " + 2971);
        expected.add ("MKC" + " " + "LAS" + " " + 14548);
        expected.add ("MKC" + " " + "MAD" + " " + 4995);
        expected.add ("MKC" + " " + "MEX" + " " + 13475);
        expected.add ("MKC" + " " + "MIA" + " " + 5990);
        expected.add ("MKC" + " " + "MKC" + " " + 0);
        expected.add ("MKC" + " " + "ORL" + " " + 170);
        expected.add ("MKC" + " " + "PHL" + " " + 11345);
        expected.add ("MKC" + " " + "SLC" + " " + 12735);
        expected.add ("MKC" + " " + "YTO" + " " + 7446);
        expected.add ("ORL" + " " + "ATL" + " " + 8964);
        expected.add ("ORL" + " " + "BNA" + " " + 121);
        expected.add ("ORL" + " " + "BOS" + " " + 7590);
        expected.add ("ORL" + " " + "CVG" + " " + 1198);
        expected.add ("ORL" + " " + "DFW" + " " + 11721);
        expected.add ("ORL" + " " + "IAH" + " " + 1676);
        expected.add ("ORL" + " " + "LAS" + " " + 13253);
        expected.add ("ORL" + " " + "MAD" + " " + 3700);
        expected.add ("ORL" + " " + "MEX" + " " + 12180);
        expected.add ("ORL" + " " + "MIA" + " " + 4695);
        expected.add ("ORL" + " " + "MKC" + " " + 13650);
        expected.add ("ORL" + " " + "ORL" + " " + 0);
        expected.add ("ORL" + " " + "PHL" + " " + 10050);
        expected.add ("ORL" + " " + "SLC" + " " + 11440);
        expected.add ("ORL" + " " + "YTO" + " " + 6151);
        expected.add ("PHL" + " " + "ATL" + " " + 13680);
        expected.add ("PHL" + " " + "BNA" + " " + 4837);
        expected.add ("PHL" + " " + "BOS" + " " + 12306);
        expected.add ("PHL" + " " + "CVG" + " " + 5914);
        expected.add ("PHL" + " " + "DFW" + " " + 597);
        expected.add ("PHL" + " " + "IAH" + " " + 6392);
        expected.add ("PHL" + " " + "LAS" + " " + 2129);
        expected.add ("PHL" + " " + "MAD" + " " + 8416);
        expected.add ("PHL" + " " + "MEX" + " " + 1056);
        expected.add ("PHL" + " " + "MIA" + " " + 9411);
        expected.add ("PHL" + " " + "MKC" + " " + 2526);
        expected.add ("PHL" + " " + "ORL" + " " + 3591);
        expected.add ("PHL" + " " + "PHL" + " " + 0);
        expected.add ("PHL" + " " + "SLC" + " " + 316);
        expected.add ("PHL" + " " + "YTO" + " " + 10867);
        expected.add ("SLC" + " " + "ATL" + " " + 13246);
        expected.add ("SLC" + " " + "BNA" + " " + 4403);
        expected.add ("SLC" + " " + "BOS" + " " + 11872);
        expected.add ("SLC" + " " + "CVG" + " " + 5480);
        expected.add ("SLC" + " " + "DFW" + " " + 163);
        expected.add ("SLC" + " " + "IAH" + " " + 5958);
        expected.add ("SLC" + " " + "LAS" + " " + 1695);
        expected.add ("SLC" + " " + "MAD" + " " + 7982);
        expected.add ("SLC" + " " + "MEX" + " " + 622);
        expected.add ("SLC" + " " + "MIA" + " " + 8977);
        expected.add ("SLC" + " " + "MKC" + " " + 2092);
        expected.add ("SLC" + " " + "ORL" + " " + 3157);
        expected.add ("SLC" + " " + "PHL" + " " + 14332);
        expected.add ("SLC" + " " + "SLC" + " " + 0);
        expected.add ("SLC" + " " + "YTO" + " " + 10433);
        expected.add ("YTO" + " " + "ATL" + " " + 1484);
        expected.add ("YTO" + " " + "BNA" + " " + 8481);
        expected.add ("YTO" + " " + "BOS" + " " + 110);
        expected.add ("YTO" + " " + "CVG" + " " + 9558);
        expected.add ("YTO" + " " + "DFW" + " " + 4241);
        expected.add ("YTO" + " " + "IAH" + " " + 10036);
        expected.add ("YTO" + " " + "LAS" + " " + 5773);
        expected.add ("YTO" + " " + "MAD" + " " + 12060);
        expected.add ("YTO" + " " + "MEX" + " " + 4700);
        expected.add ("YTO" + " " + "MIA" + " " + 13055);
        expected.add ("YTO" + " " + "MKC" + " " + 6170);
        expected.add ("YTO" + " " + "ORL" + " " + 7235);
        expected.add ("YTO" + " " + "PHL" + " " + 2570);
        expected.add ("YTO" + " " + "SLC" + " " + 3960);
        expected.add ("YTO" + " " + "YTO" + " " + 0);
        
        tests.addTestCase("travel time using deltaCycle",
                () -> allTravelTimes (deltaCycle), expected);

        for (int i = 5; i <= 50; i += 5) {
            final int n = i;
            tests.addTestCase("benchmark2(" + i + ")",
                    () -> { benchmark2(n); return true; }, true);
        }

        tests.runTests();
    }

}
