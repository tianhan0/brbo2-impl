package brbo.benchmarks.sas21.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class GetShortClassName extends Common {
  void main(int className) {
    if (className <= 0) {
      return;
    }
    int arrayPrefix = 0;
    int R = 0;
    boundAssertion("most", R <= 2 * className);
    boundAssertion("less", R <= MAX * className + MAX);
    int className_ = className;
    while (ndInt() == 0 && className_ > 0) {
      className_--;
      arrayPrefix += 2;
      R = R + 2;
    }
  }
}
