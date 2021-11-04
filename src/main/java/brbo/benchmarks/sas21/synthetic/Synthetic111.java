package brbo.benchmarks.sas21.synthetic;
import brbo.benchmarks.Common;
public abstract class Synthetic111 extends Common {
  void f(int n) {
    if (n <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= (1 * n + n));
    lessPreciseBound(R <= (1 * n + n) * 8);
    for (int it0 = n, entry0 = ndInt2(1, it0); it0 > 0; it0 -= entry0, entry0 = ndInt2(1, it0)) {
      R = R + 1;
    }
    R = R + n;
  }
}