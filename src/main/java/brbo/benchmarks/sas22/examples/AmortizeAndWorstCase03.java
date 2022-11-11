package brbo.benchmarks.sas22.examples;

import brbo.benchmarks.Common;

public abstract class AmortizeAndWorstCase03 extends Common {
    void main(int array, int costA, int costB, int costC, int n) {
        int i = 0;
        int it = 0;
        int e1 = 0;
        int e2 = 0;
        int R = 0;
        mostPreciseBound(R <= n * costB + n * array * costA + n * array);
        lessPreciseBound(R <= MAX * n * costB + MAX * n * array * costA + MAX * n * array);
        while (i < n) {
            it = array;
            if (i == 0) e1 = costA;
            else e1 = costB;
            R = R + e1;
            while (it > 0) {
                e2 = ndInt2(1, it);
                R = R + e2;
                R = R + costC;
                it -= e2;
            }
            i++;
        }
        // Amortize the first use and apply the worst-case reasoning on the second and third use
    }
}
