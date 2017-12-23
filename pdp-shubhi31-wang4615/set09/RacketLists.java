import java.util.ArrayList;

// A RacketLists is a factory class used to create empty Racketlist
public class RacketLists  {

    // Given: no arguments.
    // Returns: an empty RacketList<E>.
    public static <E> RacketList<E> empty() {
        return new MyRacketList<E>();
    }
}


// A MyRacketList is a class that implements the RacketList Interface
// and provides definition for all operations that can be performed on this
// RacketList
class MyRacketList<E> implements RacketList<E> {

    private ArrayList<E> list;

    // Constructor for MyRacketList class.
    MyRacketList () {
        this.list = new ArrayList<E>();
    }

    // Return true iff this list is empty.
    public boolean isEmpty () {
        return this.list.size() == 0;
    }

    // Where: this list is non-empty.
    // Returns: first element of this list.
    public E first () {
        return this.list.get(0);

    }

    // Where: this list is non-empty.
    // Returns: rest of this list.
    public RacketList<E> rest () {

        ArrayList<E> newArrayList = new ArrayList<E>(this.list);
        newArrayList.remove(0);
        return new MyRacketList<E>(newArrayList);
    }

    private MyRacketList(ArrayList<E> list) {
        this.list = list;
    }

    // Given: an arbitrary value x of type E.
    // Returns: a list whose first element is x and whose
    //     rest is this list.
    public RacketList<E> cons(E x) {
        ArrayList<E> newArrayList = new ArrayList<E>(this.list);
        newArrayList.add(0, x);
        return new MyRacketList<E>(newArrayList);
    }

}