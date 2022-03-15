package brbo.benchmarks.sas21.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class Join extends Common {
  void main(int array, int startIndex, int endIndex) {
    if (array <= 0 || startIndex <= 0 || endIndex <= 0 || endIndex - startIndex <= 0) {
      return;
    }
    int noOfItems = endIndex - startIndex;
    int R = 0;
    boundAssertion("most", R <= 1 + 2 * (endIndex - startIndex));
    boundAssertion("less", R <= MAX + MAX * endIndex + MAX * startIndex + MAX * array);
    int buf = 0;
    buf++;
    R = R + 1;
    for (int i = startIndex + 1; i < endIndex; i++) {
      buf++;
      R = R + 1;
      buf++;
      R = R + 1;
    }
  }
}
