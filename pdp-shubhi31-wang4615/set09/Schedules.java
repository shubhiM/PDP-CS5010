
import java.util.LinkedList;
import java.util.List;


// A Schedule class is the class that represents flights scheduling.
// It encapsulates methods to manage flight scheduling.

public class Schedules {

    // Given: the names of two airports, ap1 and ap2 (respectively),
    //     and a RacketList<Flight>; that describes all of the flights a
    //     traveller is willing to consider taking.
    // Returns: true if and only if it is possible to fly from the
    //     first airport (ap1) to the second airport (ap2) using
    //     only the given flights.
    // Examples:
    //     canGetThere ("06N", "JFK", FlightExamples.panAmFlights)  =>  false
    //     canGetThere ("JFK", "JFK", FlightExamples.panAmFlights)  =>  true
    //     canGetThere ("06N", "LAX", FlightExamples.deltaFlights)  =>  false
    //     canGetThere ("LAX", "06N", FlightExamples.deltaFlights)  =>  false
    //     canGetThere ("LGA", "PDX", FlightExamples.deltaFlights)  =>  true
    // Design Strategy: cases on the input.
    public static boolean canGetThere (String ap1,
                                       String ap2,
                                       RacketList<Flight> flights) {

        if(ap1.equals(ap2)) {
            return true;
        } else {
            Schedules schedule = new Schedules();
            List<State> finalStates = schedule.bfs(ap1, ap2, flights);
            return finalStates.size() > 0;
        }

    }

    // Given: the names of two airports, ap1 and ap2 (respectively),
    //     and a RacketList<Flight> that describes all of the flights a
    //     traveller is willing to consider taking.
    // Where: it is possible to fly from the first airport (ap1) to
    //     the second airport (ap2) using only the given flights.
    // Returns: a list of flights that tells how to fly from the
    //     first airport (ap1) to the second airport (ap2) in the
    //     least possible time, using only the given flights.
    // Examples:
    //     fastestItinerary ("JFK", "JFK", FlightExamples.panAmFlights)
    //         =>  RacketLists.empty()
    //
    //     fastestItinerary ("LGA", "PDX", FlightExamples.deltaFlights)
    // =>  RacketLists.empty().cons
    //         (Flights.make ("Delta 2163",
    //                        "MSP", "PDX",
    //                        UTCs.make (15, 0), UTCs.make (19, 2))).cons
    //             (Flights.make ("Delta 0121",
    //                            "LGA", "MSP",
    //                            UTCs.make (11, 0),
    //                            UTCs.make (14, 9)))
    // Design strategy: cases on input.
    public static RacketList<Flight> fastestItinerary (
            String ap1,
            String ap2,
            RacketList<Flight> flights) {
        Schedules schedule = new Schedules();
        List<State> finalStates = schedule.bfs(ap1, ap2, flights);
        State minState = finalStates.get(0);

        for (State s : finalStates) {
            if (minState.getTime() > s.getTime()) {
                minState = s;
            }
        }
        return schedule.convertToRacketList(minState.getPath());
    }

    // Given: the names of two airports, ap1 and ap2 (respectively),
    //     and a RacketList<Flight> that describes all of the flights a
    //     traveller is willing to consider taking.
    // Where: it is possible to fly from the first airport (ap1) to
    //     the second airport (ap2) using only the given flights.
    // Returns: the number of minutes it takes to fly from the first
    //     airport (ap1) to the second airport (ap2), including any
    //     layovers, by the fastest possible route that uses only
    //     the given flights/
    // Examples:
    //     travelTime ("JFK", "JFK", FlightExamples.panAmFlights)  =>  0
    //     travelTime ("LGA", "PDX", FlightExamples.deltaFlights)  =>  482
    // Design strategy: cases on input.
    public static int travelTime (String ap1,
                                  String ap2,
                                  RacketList<Flight> flights) {
        Schedules schedule = new Schedules();
        List<State> finalStates = schedule.bfs(ap1, ap2, flights);

        if(ap1.equals(ap2)) return 0;

        State minState = finalStates.get(0);

        for (State s : finalStates) {
            if (minState.getTime() > s.getTime()) {
                minState = s;
            }
        }

        return minState.getTime();
    }

    // Given: ap1 is the airport the traveller starts from,
    //     ap2 is the airport the traveller wants to go,
    //     flights represents a list of flights of all of the flights a
    //     traveller is willing to consider taking.
    // Returns : a list of state in which each State corresponds to a certain
    //     path the traveller can take to reach the destination.
    // Examples: 
    //     bfs("BOS", "TEN", deltaFlights) => new LinkedList<State>()
    //     bfs("BZN", "LAX", deltaFlights) => 
    //       new LinkedList<State>(Arrays.asList(
    //       new State("LAX", 
    //                 new LinkedList<Flight>(
    //                 Arrays.asList(FlightExamples.flt ("Delta 3550", "BZN",
    //                 "LAX", 2020, 2302))) ,202))
    // Design Strategy: use cases on the input flights.
    public List<State> bfs(
            String ap1, String ap2, RacketList<Flight> flights) {
        List<State> states = new LinkedList<State>();
        List<State> res = new LinkedList<State>();
        List<Flight> f = new LinkedList<Flight>();
        while(!flights.isEmpty()) {
            f.add(flights.first());
            flights = flights.rest();
        }
        if(ap1.equals(ap2)) return res;
        State initial = new State(ap1);
        states.add(initial);

        search(states, f, ap2);

        for(int i = 0; i < states.size(); i++) {
            if(states.get(i).getAirport().equals(ap2)) {
                res.add(states.get(i));
            }
        }

        return res;
    }


    // Given: states is a list of states,
    //     flights is a list of flights the traveler is willing to take.
    //     des is the airport the traveler wants to go.
    // Returns: void
    //     this function searches for possible flights to take 
    //     and adds the flights to relevant states. This function
    //     also removes flights being taken from the list of flights.
    // Design Strategy: Iterate through inputs.
    private void search(List<State> states, List<Flight> flights, String des) {
        boolean noMoreFlights = true;
        List<State> oldStates = new LinkedList<State>(states);
        List<Flight> usedFlights = new LinkedList<Flight>();
        states.clear();
        while(!oldStates.isEmpty()) {
            State s = oldStates.remove(0);
            if(s.getAirport().equals(des)) {
                states.add(s);
            } else {
                List<Flight> nextFlights = getNextFlights(s, flights);
                for (Flight f : nextFlights) {
                    noMoreFlights = false;
                    usedFlights.add(f);
                    State newState = s.copy();
                    newState.add(f);
                    addToStates(states, newState);
                }
            }
        }

        for(Flight f : usedFlights) {
            flights.remove(f);
        }

        if(noMoreFlights) return;
        search(states, flights, des);
    }

    // Given: a list of states and a new state.
    // Returns: void
    //     the function adds the new state to the list of states.
    //     If two or more states end up with the same flight, only the one
    //     with shortest time is kept while others are discarded.
    private void addToStates(List<State> states, State newState) {

        List<State> copyStates = new LinkedList<State>(states);
        State best = newState;
        Flight lastFlightTaken = newState.getLastFlight();
        for (State s : copyStates) {
            Flight lastFlightTakenS = s.getLastFlight();
            if(lastFlightTaken.isEqual(lastFlightTakenS)) {
                states.remove(s);
                if( s.getTime() < best.getTime()) {
                    best = s;
                }
            }
        }
        states.add(best);
    }

    // Given: a state s and a list of flights.
    // Returns: a list of flight in which the flights can be taken from the 
    //   given state.
    private List<Flight> getNextFlights(State s, List<Flight> flights) {
        List<Flight> re = new LinkedList<Flight>();
        if(flights.isEmpty()) return re;
        for(Flight f : flights) {
            if(f.departs().equals(s.getAirport())) {
                re.add(f);
            }
        }
        return re;
    }

    // Given: a list of flights.
    // Returns: a list of flights like the given one but is an
    //   object of RacketList.
    private RacketList<Flight> convertToRacketList(List<Flight> flights) {

        RacketList<Flight> flightsRacketList = RacketLists.empty();
        for (int i=flights.size()-1; i>=0; i--) {
            flightsRacketList = flightsRacketList.cons(flights.get(i));
        }
        return flightsRacketList;
    }
}


// A ScheduleTest is a class representing all the test cases for the Schedule
// class.

class ScheduleTest {

    // Instantiates the sample flights from FlightExamples and executes
    // tests for canGetThere, fastestItinerary and travelTime
    public static void main(String[] args) {
        RacketList<Flight> deltaFlights = FlightExamples.deltaFlights;
        RacketList<Flight> deltaCycles = FlightExamples.deltaCycle;
        RacketList<Flight> panAmFlights = FlightExamples.panAmFlights;
        Schedules s = new Schedules();
        List<State> result = s.bfs("BZN", "LAX", deltaFlights);
        for(State ss : result) {
            List<Flight> p = ss.getPath();
            System.out.println(ss.getTime());
            for(Flight f : p) {
                System.out.println(f.name());
            }
            System.out.println(".  ");
        }
        assert Schedules.canGetThere(
                "BOS",
                "TEN",
                deltaFlights) == false : "No path exists from BOS to TEN";
        assert Schedules.canGetThere(
                "LGA",
                "LGA",
                deltaFlights) == true : "Path exists from LGA to LGA";
        assert Schedules.canGetThere(
                "A",
                "TEN",
                deltaFlights) == false : "No path exists from A to TEN";
        assert Schedules.canGetThere(
                "BOS",
                "B",
                deltaFlights) == false: "No path exists from BOS to B";
        assert Schedules.canGetThere(
                "A",
                "B",
                deltaFlights) == false: "No path exists from A to B";
        assert Schedules.canGetThere(
                "BOS",
                "MSP",
                deltaFlights) == true: "Path exists from BOS to MSP";
        assert Schedules.canGetThere(
                "BOS",
                "MSP",
                panAmFlights) == false: "No Path exists from BOS to MSP";


        RacketList<Flight> re1 = Schedules.fastestItinerary(
                "PDX", "LHR", deltaFlights);
        RacketList<Flight> ex1 = RacketLists.empty();
        ex1 = ex1.cons(Flights.make(
                "Delta 0010",
                "DEN",
                "LHR",
                UTCs.make(20, 30), UTCs.make(9, 45)))
                .cons(Flights.make(
                        "Delta 4574",
                        "LAX",
                        "DEN",
                        UTCs.make(17, 35), UTCs.make(20, 7)))
                .cons(Flights.make(
                        "Delta 0950",
                        "PDX",
                        "LAX",
                        UTCs.make(14, 18), UTCs.make(16, 55)));

        assert ScheduleTest.compareRacketList(
                re1, ex1) == true : "there is a path";

        assert Schedules.travelTime(
                "ATL",
                "ATL",
                deltaCycles) == 0 : "Travel time from ATL to ATL";
        assert Schedules.travelTime(
                "BNA",
                "ATL",
                deltaCycles) == 7844: "Travel time from BNA to ATL";
        assert Schedules.travelTime(
                "BOS",
                "CVG",
                deltaCycles) == 8263: "Travel time from BOS to CVG";
        assert Schedules.travelTime(
                "LAS",
                "MKC",
                deltaCycles) == 170 : "Travel time from LAS to MKC";
        assert Schedules.travelTime(
                "MEX",
                "YTO",
                deltaCycles) == 9006: "Travel time from MEX to YTO";

    }

    // Given: two RacketLists
    // Returns: true iff the given RacketLists are exactly same.
    private static boolean compareRacketList (
            RacketList<Flight> l1, RacketList<Flight> l2) {
        while(!l1.isEmpty() && !l2.isEmpty()) {
            if(!l1.first().isEqual(l2.first())) return false;
            l1 = l1.rest();
            l2 = l2.rest();
        }
        if(l1.isEmpty() && l2.isEmpty()) return true;
        return false;
    }
}


