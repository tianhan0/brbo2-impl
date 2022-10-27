package brbo.benchmarks.sas22.test;

import brbo.benchmarks.Common;

public abstract class Test01 extends Common {
    void main(int[] text, int costA, int decision) {
        upperBound(0, "most", arraySum(text));
        if (decision > 100) {
            int i = 0;
            while (i < arrayLength(text)) {
                use(0, arrayRead(text, i));
                i++;
            }
        }
        else {
            use(0, costA);
        }
    }
}
