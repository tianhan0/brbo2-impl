package brbo.benchmarks.sas21.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class SplitWorker extends Common {
  void main(int str, boolean preserveAllTokens) {
    if (str <= 0) {
      return;
    }
    int R = 0;
    boundAssertion("most", R <= str);
    boundAssertion("less", R <= MAX * str + MAX);
    int list = 0;
    int i = 0;
    int start = 0;
    boolean match = false;
    boolean lastMatch = false;
    while (i < str) {
      if (ndInt() == 0) {
        if (match || preserveAllTokens) {
          list += i - start;
          R = R + (i - start);
          match = false;
          lastMatch = true;
        }
        ++i;
        start = i;
        continue;
      }
      lastMatch = false;
      match = true;
      i++;
    }
    if (match || preserveAllTokens && lastMatch) {
      list += i - start;
      R = R + (i - start);
    }
  }
}
