// Returns the alternating sum of the first n squares, computed ITERS times.
//
// Result is
//
//     n*n - (n-1)*(n-1) + (n-2)*(n-2) - (n-3)*(n-3) + ...
//
// Usage:
//
//     java Sumsq N ITERS

class Sumsq {

    public static void main (String[] args) {
        long n = Long.parseLong (args[0]);
        long iters = Long.parseLong (args[1]);
        System.out.println (mainLoop (n, iters));
    }

    static long mainLoop (long n, long iters) {
        if (iters == 0)
            return 0-1;
        else if (iters == 1)
            return sumSquares (n);
        else {
            sumSquares (n);
            return mainLoop (n, iters - 1);
        }
    }

    // Returns alternating sum of the first n squares.

    static long sumSquares (long n) {
        return sumSquaresLoop (n, 0);
    }

    // Returns alternating sum of the first n+1 squares, plus sum.

    static long sumSquaresLoop (long n, long sum) {
        if (n < 2)
            return sum + n * n;
        else return sumSquaresLoop2 (n - 1, sum + n * n);
    }

    // Returns alternating sum of the first n+1 squares,
    // minus (n+1)^2, plus sum.

    static long sumSquaresLoop2 (long n, long sum) {
        if (n < 2)
            return sum - n * n;
        else return sumSquaresLoop (n - 1, sum - n * n);
    }
}
