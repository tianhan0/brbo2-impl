package brbo.benchmarks.sas21.synthetic;
import brbo.benchmarks.Common;
public abstract class Synthetic022 extends Common {
  void main(int n) {
    if (n <= 0) {
      return;
    }
    int R = 0;
    boundAssertion("most", R <= (n + 1));
    boundAssertion("less", R <= (n + 1) * 8);
    R = R + n;
    R = R + 1;
  }
}