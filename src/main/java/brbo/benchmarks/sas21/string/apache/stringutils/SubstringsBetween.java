package brbo.benchmarks.sas21.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class SubstringsBetween extends Common {
  void main(int str, int open, int close) {
    if (str <= 0 || open <= 0 || close <= 0) {
      return;
    }
    int R = 0;
    boundAssertion("most", R <= str);
    boundAssertion("less", R <= MAX * str + MAX);
    int list = 0;
    int pos = 0;
    // Move this declaration outside the loop, so that the symbolic execution can be simple (i.e., it doesn't have to
    // deal with declaring a variable twice, if the declaration is inside the loop)
    int start = 0;
    int end = 0;
    while (pos < str - close) {
      if (ndBool()) {
        start = -1;
      } else {
        start = ndInt2(pos, str - 1);
      }
      if (start < 0) {
        break;
      }
      start += open;
      if (ndBool()) {
        end = -1;
      } else {
        end = ndInt2(start, str - 1);
      }
      if (end < 0) {
        break;
      }
      list += end - start;
      R = R + (end - start);
      pos = end + close;
    }
  }
}

