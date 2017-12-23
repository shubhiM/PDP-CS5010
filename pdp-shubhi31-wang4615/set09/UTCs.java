
// A UTCs is a factory class that create UTC objects

public class UTCs {

	// Given: the hour in [0,23] and the minute in [0,59]
	// Returns: a UTC object with that hour and minute
	public static UTC make (int h, int m) {
		return new MyUTC(h, m);
	}

}


// A MyUTC class is a class that implements UTC interface
class MyUTC implements UTC {
	private int hour;
	private int minute;

	// Constructor for MyUTC class
	// Given: the hour in [0,23] and the minute in [0,59]
	public MyUTC(int hour, int minute) {
		this.hour = hour;
		this.minute = minute;
	}

	// Returns: the hour, between 0 and 23 inclusive.
	public int hour() {
		return this.hour;
	}

	// Returns: the minute, between 0 and 59 inclusive.
	public int minute() {

		return this.minute;
	}

	// Given: an UTC t2.
    // Returns: true iff the given UTC is equal to this UTC.
	public boolean isEqual(UTC utc) {
		return (this.hour == utc.hour())&&(this.minute == utc.minute());
	}
}