package brbo.benchmarks.sas22.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class SubstringsBetween extends Common {
  void execute(int str) {
    if (str <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= str);
    lessPreciseBound(R <= MAX * str + MAX);
    int list = 0;
    int pos = 0;
    int open = 1;
    int close = ndInt2(0, str - 1);
    while (pos < str - close) {
      int start = ndInt2(pos, str);
      if (start == str) {
        break;
      }
      start += open;
      int end = ndInt2(start, str);
      if (end == str) {
        break;
      }
      list += end - start;
      R = R + (end - start);
      pos = end + close;
    }
  }
}

