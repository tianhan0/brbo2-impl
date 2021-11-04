package brbo.benchmarks.sas21.synthetic;
import brbo.benchmarks.Common;
public abstract class Synthetic058 extends Common {
  void main(int n) {
    if (n <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= n * (n * (n + n) + n * (n + 1)));
    lessPreciseBound(R <= n * (n * (n + n) + n * (n + 1)) * 8);
    for (int i0 = 0; i0 < n; i0++) {
      for (int i1 = 0; i1 < n; i1++) {
        R = R + n;
        R = R + n;
      }
      for (int i2 = 0; i2 < n; i2++) {
        R = R + n;
        R = R + 1;
      }
    }
  }
}