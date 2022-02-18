package brbo.benchmarks.sas21.synthetic;
import brbo.benchmarks.Common;
public abstract class Synthetic067 extends Common {
  void main(int n) {
    if (n <= 0) {
      return;
    }
    int R = 0;
    boundAssertion("most", R <= (n + n));
    boundAssertion("less", R <= (n + n) * 8);
    R = R + n;
    R = R + n;
  }
}