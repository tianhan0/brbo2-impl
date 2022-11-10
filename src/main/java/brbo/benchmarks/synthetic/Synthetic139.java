package brbo.benchmarks.synthetic;
import brbo.benchmarks.Common;
public abstract class Synthetic139 extends Common {
  void f(int n) {
    if (n <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= n * (n + 1 * n));
    lessPreciseBound(R <= n * (n + 1 * n) * 8);
    for (int i0 = 0; i0 < n; i0++) {
      R = R + n;
      for (int it0 = n, entry0 = ndInt2(1, it0); it0 > 0; it0 -= entry0, entry0 = ndInt2(1, it0)) {
        R = R + 1;
      }
    }
  }
}