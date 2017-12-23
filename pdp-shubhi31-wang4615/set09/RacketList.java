// A RacketList<E> is an object of any class that implements RacketList<E>.
//
// Interpretation: A RacketList<E> represents an immutable
// (and possibly empty) sequence of E values, with operations
// analogous to those we've been using in Racket.

interface RacketList<E> {

    // Return true iff this list is empty
    boolean isEmpty ();

    // Where: this list is non-empty
    // Returns: first element of this list
    E first ();

    // Where: this list is non-empty
    // Returns: rest of this list
    RacketList<E> rest ();

    // Given: an arbitrary value x of type E
    // Returns: a list whose first element is x and whose
    //     rest is this list
    RacketList<E> cons (E x);
}