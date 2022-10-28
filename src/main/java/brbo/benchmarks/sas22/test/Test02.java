package brbo.benchmarks.sas22.test;

import brbo.benchmarks.Common;

public abstract class Test02 extends Common {
    void main(int[] text, int costA, int costB, boolean decision1, boolean decision2) {
        upperBound(0, "most", arraySum(text));
        if (decision1) {
            int i = 0;
            while (i < arrayLength(text)) {
                use(0, arrayRead(text, i));
                i++;
            }
        }
        else {
            use(0, costA);
        }
        if (decision2) {
            use(0, costA);
        }
        else {
            use(0, costB);
        }
    }
}
