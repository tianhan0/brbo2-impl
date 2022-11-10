package brbo.benchmarks.synthetic;
import brbo.benchmarks.Common;
public abstract class Synthetic148 extends Common {
  void f(int n) {
    if (n <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= (n * n * (n + n) + n * (1 * n + n * 1)));
    lessPreciseBound(R <= (n * n * (n + n) + n * (1 * n + n * 1)) * 8);
    for (int i0 = 0; i0 < n; i0++) {
      for (int i1 = 0; i1 < n; i1++) {
        R = R + n;
        R = R + n;
      }
    }
    for (int i2 = 0; i2 < n; i2++) {
      for (int it0 = n, entry0 = ndInt2(1, it0); it0 > 0; it0 -= entry0, entry0 = ndInt2(1, it0)) {
        R = R + 1;
      }
      for (int i3 = 0; i3 < n; i3++) {
        R = R + 1;
      }
    }
  }
}