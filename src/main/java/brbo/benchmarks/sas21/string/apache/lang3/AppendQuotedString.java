package brbo.benchmarks.sas21.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class AppendQuotedString extends Common {
  void main(int pattern, int pos) {
    if (pattern <= 0 || pos <= 0 || pattern < pos) {
      return;
    }
    int appendTo = 0;
    int R = 0;
    boundAssertion("most", R <= pattern);
    boundAssertion("less", R <= MAX * pattern + MAX * pos + MAX);
    int start = pos;
    int lastHold = start;
    for (int i = pos; i < pattern; i++) {
      if (ndInt() == 0) {
        pos++;
        appendTo += pos - lastHold;
        R = R + (pos - lastHold);
        break;
      }
      pos++;
    }
  }
}
