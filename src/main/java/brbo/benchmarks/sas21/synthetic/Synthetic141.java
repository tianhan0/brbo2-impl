package brbo.benchmarks.sas21.synthetic;
import brbo.benchmarks.Common;
public abstract class Synthetic141 extends Common {
  void main(int n) {
    if (n <= 0) {
      return;
    }
    int R = 0;
    boundAssertion("most", R <= n * (n + n * 1));
    boundAssertion("less", R <= n * (n + n * 1) * 8);
    for (int i0 = 0; i0 < n; i0++) {
      R = R + n;
      for (int i1 = 0; i1 < n; i1++) {
        R = R + 1;
      }
    }
  }
}