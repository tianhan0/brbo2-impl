package brbo.benchmarks.sas22.archive;

import brbo.benchmarks.Common;

public abstract class Test04 extends Common {
  void main(int n, int a, int b) {
    if (n <= 0 || n > LARGE_INT || a < 0 || b < 0) {
      return;
    }
    int R = 0;
    // boundAssertion("most", R <= (a + b) * 2);

    R = R + a;
    R = R + a + b;
    R = R + b;
  }
}
// Test the imprecision caused by the variations in the costs of segments