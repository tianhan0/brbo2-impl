package brbo.benchmarks.sas21.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class AppendDetail extends Common {
  void main(int array) {
    if (array <= 0) {
      return;
    }
    int buffer = 0;
    int R = 0;
    mostPreciseBound(R <= 2 * array - 1);
    lessPreciseBound(R <= MAX + MAX * array);
    for (int i = 0; i < array; i++) {
      if (i > 0) {
        buffer++;
        R = R + 1;
      }
      buffer++;
      R = R + 1;
    }
  }
}
// The initial amortization suffices.
// The success relies on if it is possible to infer precise invariants for the counter at line 17.