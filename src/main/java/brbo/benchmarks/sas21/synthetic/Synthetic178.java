package brbo.benchmarks.sas21.synthetic;
import brbo.benchmarks.Common;
public abstract class Synthetic178 extends Common {
  void main(int n) {
    if (n <= 0) {
      return;
    }
    int R = 0;
    boundAssertion("most", R <= (n * (n + n) * n + n * (1 + n)));
    boundAssertion("less", R <= (n * (n + n) * n + n * (1 + n)) * 8);
    for (int it0 = n, entry0 = ndInt2(1, it0); it0 > 0; it0 -= entry0, entry0 = ndInt2(1, it0)) {
      for (int i0 = 0; i0 < n; i0++) {
        R = R + n;
        R = R + n;
      }
      for (int i1 = 0; i1 < n; i1++) {
        R = R + 1;
        R = R + entry0;
      }
    }
  }
}