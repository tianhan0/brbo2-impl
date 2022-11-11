package brbo.benchmarks.sas22.examples;

import brbo.benchmarks.Common;

public abstract class SameExpression01 extends Common {
    void main(int costA, int costB, int n) {
        int i = 0;
        int R = 0;
        mostPreciseBound(R <= n * costA + n * costB);
        lessPreciseBound(R <= MAX * n * costA + MAX * n * costB);
        int e = 0;
        while (i < n) {
            e = costA;
            R = R + e;
            e = costB;
            R = R + e;
            i++;
        }
    }
}
