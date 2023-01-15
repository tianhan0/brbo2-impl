package brbo.benchmarks.sas22.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class AppendQuotedString extends Common {
  void execute(int pattern, int[] stopIndex) {
    if (pattern <= 0 || arrayLength(stopIndex) <= pattern) {
      return;
    }
    int appendTo = 0;
    int R = 0;
    mostPreciseBound(R <= pattern);
    lessPreciseBound(R <= MAX * pattern + MAX);
    int pos = ndInt2(0, pattern);
    int start = pos;
    int lastHold = start;
    int choice = 0;
    for (int i = pos; i < pattern; i++) {
      choice = arrayRead(stopIndex, i);
      if (choice <= BOOLEAN_SEPARATOR) {
        pos++;
        appendTo += pos - lastHold;
        R = R + (pos - lastHold);
        break;
      }
      pos++;
    }
  }
}
