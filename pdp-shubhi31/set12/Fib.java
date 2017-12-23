// Computes the Nth Fibonacci number ITERS times.
//
// Usage:
//
//     java Fib N ITERS

class Fib {

    public static void main (String[] args) {
        long n = Long.parseLong (args[0]);
        long iters = Long.parseLong (args[1]);
        System.out.println (mainLoop (n, iters));
    }

    static long mainLoop (long n, long iters) {
        if (iters == 0)
            return 0-1;
        else if (iters == 1)
            return fib (n);
        else {
            fib (n);
            return mainLoop (n, iters - 1);
        }
    }

    static long fib (long n) {
        if (n < 2)
            return n;
        else
            return fib (n - 1) + fib (n - 2);
    }
}
