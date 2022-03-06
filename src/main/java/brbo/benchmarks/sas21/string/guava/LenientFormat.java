package brbo.benchmarks.sas21.string.guava;

import brbo.benchmarks.Common;

abstract public class LenientFormat extends Common {
  void main(int template, int args) {
    if (template <= 0 || args <= 0
        || template > LARGE_INT || args > LARGE_INT) {
      return;
    }
    int builder = 0;
    int R = 0;
    boundAssertion("most", R <= template + args + 2 + 2 * args);
    boundAssertion("less", R <= MAX * template + MAX * args + MAX);
    int templateStart = 0;
    int i = 0;
    while (i < args) {
      int placeholderStart = 0;
      if (ndInt() == 0) placeholderStart = -1;
      else placeholderStart = ndInt2(templateStart, template - 1);
      if (placeholderStart == -1) {
        break;
      }
      builder += placeholderStart - templateStart;
      R = R + (placeholderStart - templateStart);
      builder++;
      R = R + 1;
      i++;
      templateStart = placeholderStart + 2;
    }
    builder += template - templateStart;
    R = R + (template - templateStart);
    if (i < args) {
      builder++;
      R = R + 1;
      builder++;
      R = R + 1;
      i++;
      while (i < args) {
        builder++;
        R = R + 1;
        builder++;
        R = R + 1;
        i = i + 1;
      }
    }
  }
}
