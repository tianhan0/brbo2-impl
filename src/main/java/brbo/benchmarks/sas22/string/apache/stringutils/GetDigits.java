package brbo.benchmarks.sas22.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class GetDigits extends Common {
  void execute(int str, int[] choices) {
    if (str <= 0 || arrayLength(choices) < str) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= str);
    lessPreciseBound(R <= MAX * str);
    int strDigits = 0;
    int choice = 0;
    for (int i = 0; i < str; i++) {
      choice = arrayRead(choices, i);
      if (choice > BOOLEAN_SEPARATOR) {
        strDigits++;
        R = R + 1;
      }
    }
  }
}
