package brbo.benchmarks.sas21.synthetic;
import brbo.benchmarks.Common;
public abstract class Synthetic024 extends Common {
  void main(int n) {
    if (n <= 0) {
      return;
    }
    int R = 0;
    boundAssertion("most", R <= (1 + 1));
    boundAssertion("less", R <= (1 + 1) * 8);
    R = R + 1;
    R = R + 1;
  }
}