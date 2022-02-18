package brbo.benchmarks.sas21.synthetic;
import brbo.benchmarks.Common;
public abstract class Synthetic005 extends Common {
  void main(int n) {
    if (n <= 0) {
      return;
    }
    int R = 0;
    boundAssertion("most", R <= n * (1 + n));
    boundAssertion("less", R <= n * (1 + n) * 8);
    for (int i0 = 0; i0 < n; i0++) {
      R = R + 1;
      R = R + n;
    }
  }
}