package brbo.benchmarks.sas21.synthetic;
import brbo.benchmarks.Common;
public abstract class Synthetic165 extends Common {
  void main(int n) {
    if (n <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= n * (n + n));
    lessPreciseBound(R <= n * (n + n) * 8);
    for (int i0 = 0; i0 < n; i0++) {
      R = R + n;
      R = R + n;
    }
  }
}