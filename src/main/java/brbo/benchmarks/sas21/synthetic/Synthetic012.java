package brbo.benchmarks.sas21.synthetic;
import brbo.benchmarks.Common;
public abstract class Synthetic012 extends Common {
  void main(int n) {
    if (n <= 0) {
      return;
    }
    int R = 0;
    boundAssertion("most", R <= n);
    boundAssertion("less", R <= n * 8);
    for (int it0 = n, entry0 = ndInt2(1, it0); it0 > 0; it0 -= entry0, entry0 = ndInt2(1, it0)) {
      R = R + entry0;
    }
  }
}