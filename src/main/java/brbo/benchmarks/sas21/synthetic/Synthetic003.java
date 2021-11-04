package brbo.benchmarks.sas21.synthetic;
import brbo.benchmarks.Common;
public abstract class Synthetic003 extends Common {
  void main(int n) {
    if (n <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= n);
    lessPreciseBound(R <= n * 8);
    R = R + n;
  }
}