import java.util.LinkedList;
import java.util.List;

// A State object is an instantiation of the class State.
//
// Interpretation: A State represents a possible sequence of flights
//   the traveler can take to reach to a certain airport.
public class State {

    private List<Flight> path;
    private String ap;
    private int time;

    // Constructor for the class State.
    // Given: an airport ap.
    public State(String ap) {
        this.path = new LinkedList<Flight>();
        this.ap = ap;
        this.time = 0;
    }

    // Constructor for the class State.
    // Given: an airport ap, a list of flights, a time.
    public State(String ap, 
                 List<Flight> flights, 
                 int time) {
        this.path = new LinkedList<Flight>(flights);
        this.ap = ap;
        this.time = time;
    }

    // Returns: a new State object with values like this one.
    public State copy() {
        return new State(this.ap, this.path, this.time);
    }

    // Given: a flight f.
    // Interpretation: the function modifies this state by 
    //   adding the new flight to the state.
    public void add(Flight f) {
        if(this.path.isEmpty())
            this.time += getTime(null, f);
        else
            this.time += getTime(this.path.get(this.path.size()-1), f);

        this.path.add(this.path.size(), f);
        this.ap = f.arrives();

    }

    // Returns: the last flight taken in this state.
    public Flight getLastFlight() {
        return this.path.get(this.path.size()-1);
    }

    // Returns: the current airport of this state.
    public String getAirport() {
        return this.ap;
    }

    // Given: two flights f1 and f2.
    // Returns: the time interval between these two flights.
    private int getTime(Flight f1, Flight f2) {
        if(f1 == null) {
            return flightTime(f2);
        } else {
            return layOverTime(f1, f2) + flightTime(f2);
        }
    }

    // Returns: the travel time of this state.
    public int getTime() {return  this.time;}

    // Returns: the list of flights of this state.
    public List<Flight> getPath() {return this.path;}

    // Given: a flight f.
    // Returns: the flight time of the given flight.
    private int flightTime(Flight f) {

        return timeDifference(f.departsAt(), f.arrivesAt());
    }

    // Given: two flights f1 and f2.
    // Returns: the layover time between the given flights.
    private int layOverTime(Flight f1, Flight f2) {
        return timeDifference(f1.arrivesAt(), f2.departsAt());
    }

    // Given: two UTCs u1 and u2.
    // Returns: the time interval from u1 to u2.
    private int timeDifference(UTC u1, UTC u2) {
        int t1 = UTCtoMinutes(u1);
        int t2 = UTCtoMinutes(u2);
        if(t1 <= t2) return t2 - t1;
        return t2 + 24 * 60 - t1;
    }

    // Given: a UTC u.
    // Returns: the given UTC in minutes.
    private int UTCtoMinutes(UTC u) {
        return u.hour()*60 + u.minute();
    }
}

