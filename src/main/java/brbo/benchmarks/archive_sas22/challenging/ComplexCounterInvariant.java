package brbo.benchmarks.archive_sas22.challenging;

import brbo.benchmarks.Common;

public abstract class ComplexCounterInvariant extends Common {
    void main(int n) {
        // upperBound(0, "most", 2 * n);
        int i = 0;
        while (i < n) {
            use(0, 1);
            i++;
        }
        i = 0;
        while (i < n) {
            use(0, 1);
            i++;
        }
        // Since every use is treated as a segment within the same group,
        // we need to infer a complicated invariant for the counter
    }
}
