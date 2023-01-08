package brbo.benchmarks.archive_sas22.examples;

import brbo.benchmarks.Common;

public abstract class AmortizeAndWorstCase01 extends Common {
    void main(int array, int costA) {
        int i = 0;
        int R = 0;
        mostPreciseBound(R <= array + array * costA);
        lessPreciseBound(R <= MAX + MAX * array + MAX * array * costA);
        int it = array;
        int e = 0;
        while (it > 0) {
            e = ndInt2(1, it);
            R = R + e;
            R = R + costA;
            it -= e;
        }
        // Amortize the first use and apply the worst-case reasoning on the second use
    }
}
