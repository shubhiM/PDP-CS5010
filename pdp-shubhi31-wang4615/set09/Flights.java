// A Flights is a factory class to create FLight objects


public abstract class Flights  {

    // Given: name represents the name of the flight, departs represents
    //   name of the airport the flight departs, arrives represents the 
    //   name of the airport the flight arrives, departsAt represents
    //   the UTC the flight departs, arrivesAt represents the UTC the 
    //   flight arrives.
    // Returns: a new Flight object
    public static Flight make (
            String s, String ap1, String ap2, UTC t1, UTC t2) {
        return new MyFlight(s, ap1, ap2, t1, t2);
    }
}


// A MyFlight is a class that implements Flight Interface.

class MyFlight implements Flight {

    private String name;
    private String departs;
    private String arrives;
    private UTC departsAt;
    private UTC arrivesAt;

    // Constructor for class MyFlight
    // Given: name represents the name of the flight, departs represents
    //   name of the airport the flight departs, arrives represents the 
    //   name of the airport the flight arrives, departsAt represents
    //   the UTC the flight departs, arrivesAt represents the UTC the 
    //   flight arrives.
    MyFlight (String name,
              String departs,
              String arrives, 
              UTC departsAt, 
              UTC arrivesAt) {

        this.name = name;
        this.departs  = departs;
        this.arrives = arrives;
        this.departsAt = departsAt;
        this.arrivesAt = arrivesAt;
    }

    // Return: the name of this flight.
    public String name() {
        return this.name;
    }
    
    // Return: the name of the airport at which this flight departs.
    public String departs () {
        return this.departs;
    }

    // Return: the name of the airport at which this flight arrives.
    public String arrives () {
        return this.arrives;
    }

    // Return: the time at which this flight departs.
    public UTC departsAt () {
        return this.departsAt;
    }

    // Return: the time at which this flight arrives.
    public UTC arrivesAt () {
        return this.arrivesAt;
    }

    // Given: a Flight f2 
    // Return: true if and only if Flight f2 is equal to this flight
    public boolean isEqual(Flight f2) {
        return (this.name.equals(f2.name()) &&
                this.arrives.equals(f2.arrives()) &&
                this.departs.equals(f2.departs()) &&
                this.departsAt.isEqual(f2.departsAt()) &&
                this.arrivesAt.isEqual(f2.arrivesAt()));
    }
}