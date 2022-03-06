package brbo.benchmarks.sas21.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class ParseToken extends Common {
  void main(int pattern) {
    if (pattern <= 0) {
      return;
    }
    int buf = 0;
    int R = 0;
    boundAssertion("most", R <= pattern + 1);
    boundAssertion("less", R <= MAX * pattern + MAX);
    int i = ndInt2(0, pattern - 1);
    if (ndInt() == 0) {
      buf++;
      R = R + 1;
      while (i + 1 < pattern) {
        if (ndInt() == 0) {
          buf++;
          R = R + 1;
        }
        else {
          break;
        }
      }
    }
    else {
      buf++;
      R = R + 1;
      for (; i < pattern; i++) {
        if (ndInt() == 0) {
          if (ndInt() == 0) {
            i++;
            buf++;
            R = R + 1;
          }
        }
        else {
          if (ndInt() == 0) {
            i--;
            break;
          }
          else {
            buf++;
            R = R + 1;
          }
        }
      }
    }
  }
}
