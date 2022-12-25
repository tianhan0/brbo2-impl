package brbo.benchmarks.sas21.synthetic;
import brbo.benchmarks.Common;
public abstract class Synthetic007 extends Common {
  void f(int n) {
    if (n <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= (1 + n * n * 1));
    lessPreciseBound(R <= (1 + n * n * 1) * 8);
    R = R + 1;
    for (int i0 = 0; i0 < n; i0++) {
      for (int i1 = 0; i1 < n; i1++) {
        R = R + 1;
      }
    }
  }
}