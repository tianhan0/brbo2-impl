package brbo.benchmarks.sas21.synthetic;
import brbo.benchmarks.Common;
public abstract class Synthetic029 extends Common {
  void main(int n) {
    if (n <= 0) {
      return;
    }
    int R = 0;
    boundAssertion("most", R <= (n * (n + n) + n));
    boundAssertion("less", R <= (n * (n + n) + n) * 8);
    for (int i0 = 0; i0 < n; i0++) {
      R = R + n;
      R = R + n;
    }
    R = R + n;
  }
}