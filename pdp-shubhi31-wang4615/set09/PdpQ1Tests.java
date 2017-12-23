public class PdpQ1Tests {
    public static Boolean utcsEqual(UTC a, UTC b) {
        return a.hour() == b.hour() && a.minute() == b.minute();
    }
    
    public static int length(RacketList<?> list) {
        int n = 0;
        while (!list.isEmpty()) {
            n++;
            list = list.rest();
        }
        return n;
    }

    public static void main(String[] args) {
        PdpTestSuite tests = new PdpTestSuite(10);
        int[][] times = {{0, 0}, {0, 59}, {23, 0}, {23, 59}, {15, 31}};

        // Test UTC fields
        for (int[] t : times) {
            tests.addTestCase(
                    String.format("UTC.hour %d:%02d", t[0], t[1]),
                    () -> UTCs.make(t[0], t[1]).hour(),
                    t[0]);
            tests.addTestCase(
                    String.format("UTC.minute %d:%02d", t[0], t[1]),
                    () -> UTCs.make(t[0], t[1]).minute(),
                    t[1]);
        }

        // Test UTC.isEqual
        for (int[] t : times) {
            tests.addTestCase(
                    String.format("UTC.isEqual reflexive %d:%02d", t[0], t[1]),
                    () -> UTCs.make(t[0], t[1]).isEqual(UTCs.make(t[0], t[1])),
                    true);
            int m2 = (t[1] + 1) % 60;
            tests.addTestCase(
                    String.format("UTC.isEqual unequal %d:%02d %d:%02d", t[0], t[1], t[0], m2),
                    () -> UTCs.make(t[0], t[1]).isEqual(UTCs.make(t[0], m2)),
                    false);
        }

        UTC t0 = UTCs.make(3, 14);
        UTC t1 = UTCs.make(4, 2);
        UTC t2 = UTCs.make(5, 3);
        Flight f1 = Flights.make("Pigeon Airways 5010", "BOS", "SFO", t0, t1);
        Flight f2 = Flights.make("Pigeon Airways 5010", "BOS", "SFO", t0, t1);
        Flight f3 = Flights.make("Pigeon Airways 5010", "BOS", "SFO", t0, t2);
        Flight f4 = Flights.make("Kangaroo Airways 5011", "BOS", "SFO", t0, t1);
        Flight f5 = Flights.make("Pigeon Airways 5010", "LGA", "SFO", t0, t1);

        // Test flight fields
        tests.addTestCase("Flight.name", () -> f1.name(), "Pigeon Airways 5010");
        tests.addTestCase("Flight.departs", () -> f1.departs(), "BOS");
        tests.addTestCase("Flight.arrives", () -> f1.arrives(), "SFO");
        tests.addTestCase("Flight.departsAt", () -> f1.departsAt(), t0,
                PdpQ1Tests::utcsEqual);
        tests.addTestCase("Flight.arrivesAt", () -> f1.arrivesAt(), t1,
                PdpQ1Tests::utcsEqual);

        // Test flights that pass midnight
        tests.addTestCase("red-eye flight 1",
                () -> Flights.make("Owl Air", "JFK", "LAX",
                        UTCs.make(21, 23), UTCs.make(0, 0)) == null,
                        false);
        tests.addTestCase("red-eye flight 2",
                () -> Flights.make("Owl Air", "JFK", "LAX",
                        UTCs.make(21, 23), UTCs.make(2, 1)) == null,
                        false);

        // Test Flight.isEqual
        tests.addTestCase("Flight.isEqual 2", () -> f1.isEqual(f2), true);
        tests.addTestCase("Flight.isEqual 3", () -> f1.isEqual(f3), false);
        tests.addTestCase("Flight.isEqual 4", () -> f1.isEqual(f4), false);
        tests.addTestCase("Flight.isEqual 5", () -> f1.isEqual(f5), false);
        
        // Test RacketList
        tests.addTestCase("empty list is empty",
                () -> RacketLists.empty().isEmpty(), true);
        tests.addTestCase("singleton list is not empty,",
                () -> RacketLists.empty().cons("oh hai").isEmpty(), false);
        tests.addTestCase("two-element list is not empty,",
                () -> RacketLists.empty().cons("oh").cons("hai").isEmpty(), false);
        tests.addTestCase("number of elements",
                () -> {
                    RacketList<Integer> r = RacketLists.empty();
                    for (int i = 10; i > 0; i--) {
                        r = r.cons(i);
                    }
                    return length(r);
                }, 10);
        tests.addTestCase("numbers in, numbers out",
                () -> {
                    RacketList<Integer> r = RacketLists.empty();
                    for (int i = 10; i > 0; i--) {
                        r = r.cons(i);
                    }
                    for (int i = 1; i <= 10; i++) {
                        if (r.first().intValue() != i)
                            return false;
                        r = r.rest();
                    }
                    return r.isEmpty();
                }, true);
        tests.addTestCase("cons() does not change existing list 1",
                () -> {
                    RacketList<Integer> r1 = RacketLists.<Integer>empty();
                    r1.cons(43);
                    return length(r1);
                }, 0);
        tests.addTestCase("cons() does not change existing list 2",
                () -> {
                    RacketList<Integer> r1 = RacketLists.<Integer>empty().cons(42);
                    r1.cons(43);
                    return length(r1);
                }, 1);
        tests.addTestCase("rest() does not change existing list",
                () -> {
                    RacketList<Integer> r1 = RacketLists.<Integer>empty().cons(42);
                    r1.rest();
                    return length(r1);
                }, 1);
        tests.addTestCase("now that's just weird",
                () -> {
                    RacketList<Object> list = RacketLists.empty();
                    for (int i = 0; i < 20; i++) {
                        list = list.cons(list);
                    }
                    return length(list);
                }, 20);
        
        tests.runTests();
    }
}
