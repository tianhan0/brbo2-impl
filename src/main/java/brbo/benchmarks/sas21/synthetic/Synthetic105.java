package brbo.benchmarks.sas21.synthetic;
import brbo.benchmarks.Common;
public abstract class Synthetic105 extends Common {
  void main(int n) {
    if (n <= 0) {
      return;
    }
    int R = 0;
    boundAssertion("most", R <= (n * n + n * n));
    boundAssertion("less", R <= (n * n + n * n) * 8);
    for (int it0 = n, entry0 = ndInt2(1, it0); it0 > 0; it0 -= entry0, entry0 = ndInt2(1, it0)) {
      for (int i0 = 0; i0 < entry0; i0++) {
        R = R + entry0;
      }
      for (int i1 = 0; i1 < n; i1++) {
        R = R + entry0;
      }
    }
  }
}