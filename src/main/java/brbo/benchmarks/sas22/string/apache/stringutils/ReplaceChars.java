package brbo.benchmarks.sas22.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class ReplaceChars extends Common {
  void execute(int str, int[] choices, int searchChars, int replaceChars) {
    if (str <= 0 || searchChars <= 0 || replaceChars <= 0 || arrayLength(choices) < str) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= str);
    lessPreciseBound(R <= MAX * str + MAX);
    int choice = 0;
    for (int i = 0; i < str; i++) {
      choice = arrayRead(choices, i);
      if (choice > BOOLEAN_SEPARATOR) {
        // Replace
        if (i < replaceChars) {
          R = R + 1;
        }
      } else {
        // Not replace
        R = R + 1;
      }
    }
  }
}
