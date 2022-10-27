package brbo.benchmarks.sas22.test;

import brbo.benchmarks.Common;

public abstract class TemplateEngine extends Common {
    void main(int[] text) {
        upperBound(0, "most", arraySum(text));
        upperBound(0, "less", MAX * arraySum(text) + MAX);

        int i = 0;
        while (i < arrayLength(text)) {
            use(0, arrayRead(text, i));
            i++;
            arrayRead(text, i);
            i++;
        }
    }
}
