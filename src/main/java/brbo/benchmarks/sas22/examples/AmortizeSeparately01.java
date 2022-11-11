package brbo.benchmarks.sas22.examples;

import brbo.benchmarks.Common;

public abstract class AmortizeSeparately01 extends Common {
    void main(int array1, int array2) {
        int R = 0;
        mostPreciseBound(R <= array1 + array2);
        lessPreciseBound(R <= MAX * array1 + MAX * array2);
        int it1 = array1;
        int it2 = array2;
        int e = 0;
        while (it1 > 0 && it2 > 0) {
            e = ndInt2(1, it1);
            it1 -= e;
            R = R + e;

            e = ndInt2(1, it2);
            it2 -= e;
            R = R + e;
        }
        // Separately amortize the two arrays
    }
}
