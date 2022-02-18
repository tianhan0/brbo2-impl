package brbo.benchmarks.sas22;

import brbo.benchmarks.Common;

public abstract class Test02 extends Common {
  void main(int n, int a) {
    if (n <= 0 || n > LARGE_INT || a < 0) {
      return;
    }
    int R = 0;
    boundAssertion("most", R <= n * a);

    int i = 0;
    while (i < n) {
      R = R + a;
      i++;
    }
  }
}
// Test the imprecision caused by using a linear conjunctive domain to approximate polynomials