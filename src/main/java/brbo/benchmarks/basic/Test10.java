package brbo.benchmarks.basic;

import brbo.benchmarks.Common;

abstract class Test10 extends Common {
  void f(int n) {
    if (n <= 0) {
      return;
    }
    int R1 = 0;
    int R2 = 0;
    mostPreciseBound(R1 + R2 <= n);
    lessPreciseBound(R1 + R2 <= MAX * n + MAX);
    int i = 0;
    while (i < n) {
      i++;
      R1 = R1 + 1;
      R2 = R2 + 1;
    }
  }
}