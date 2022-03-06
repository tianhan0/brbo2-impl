package brbo.benchmarks.sas21.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class Replace extends Common {
  void main(int text, int searchString, int replacement, int max) {
    if (text <= 0 || searchString <= 0 || max <= 0 || replacement <= 0) {
      return;
    }
    int dummy = 0;
    int start = 0;
    int end = 0;
    if (ndInt() == 0) {
      end = -1;
    } else {
      end = ndInt2(start, text - 1);
    }
    if (end == -1) {
      return;
    }
    int R = 0;
    boundAssertion("most", R <= text);
    boundAssertion("less", R <= MAX * text + MAX);
    int replLength = searchString;
    int buf = 0;
    while (end != -1) {
      buf += end - start;
      R = R + (end - start);
      buf += replacement;
      // R = R + replacement;
      start = end + replLength;
      max--;
      if (max == 0) {
        break;
      }
      if (ndInt() == 0) {
        end = -1;
      } else {
        ndInt2(start, text - 1);
      }
    }
    buf += text - start;
    R = R + (text - start);
  }
}
