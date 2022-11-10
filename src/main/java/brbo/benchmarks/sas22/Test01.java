package brbo.benchmarks.sas22;

import brbo.benchmarks.Common;

public abstract class Test01 extends Common {
  void main(int n, int a, int b) {
    if (n <= 0 || n > LARGE_INT || a < 0 || b < 0) {
      return;
    }
    int R = 0;
    // boundAssertion("most", R <= n * a || R <= n * b);

    int i = 0;
    while (i < n) {
      if (ndInt() == 0) {
        R = R + a;
      } else {
        R = R + b;
      }
      i++;
    }
  }
}
// Test the imprecision caused by using a linear conjunctive domain to approximate disjunction