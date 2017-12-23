// Returns the alternating sum of the first n squares, computed ITERS times.
//
// Result is
//
//     n*n - (n-1)*(n-1) + (n-2)*(n-2) - (n-3)*(n-3) + ...
//
// Usage:
//
//     java Sumsq N ITERS

class Sumsq2 {

    public static void main (String[] args) {
        long n = Long.parseLong (args[0]);
        long iters = Long.parseLong (args[1]);
        System.out.println (mainLoop (n, iters));
    }

    // Modify this method to use loop syntax instead of tail recursion.

    static long mainLoop (long n, long iters) {
        if (iters == 0)
            return 0-1;

        while(iters > 1) {
            sumSquares (n);
            iters = iters - 1;
        }
        return sumSquares (n);

    }

    // Returns alternating sum of the first n squares.

    static long sumSquares (long n) {
        return sumSquaresLoop (n, 0);
    }

    // Modify the following methods to use loop syntax
    // instead of tail recursion.

    // Returns alternating sum of the first n+1 squares, plus sum.

    static long sumSquaresLoop (long n, long sum) {

        boolean cond = false;

        while(n >=2) {
            if(! cond) {
                sum = sum + n * n;
                cond = true;
            } else {

                sum = sumSquaresLoop2(n , sum);
                cond = false;
            }
            n = n - 1;
        }

        if (n < 2 && !cond) {
            sum = sum + n * n;
            return sum;
        }
        else {
            sum = sumSquaresLoop2(n , sum);
            return sum;
        }
        
    }

    // Returns alternating sum of the first n+1 squares,
    // minus (n+1)^2, plus sum.

    static long sumSquaresLoop2 (long n, long sum) {
        return sum - n * n;
    }
}
