package brbo.benchmarks.sas22.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class Join2 extends Common {
  void execute(int n, int[] choices) {
    if (n <= 0 || arrayLength(choices) < n) {
      return;
    }
    int iterator = n;
    int R = 0;
    mostPreciseBound(R <= 1 + 2 * n);
    lessPreciseBound(R <= MAX + MAX * n);
    int buf = 0;
    iterator--;
    if (iterator <= 0) {
      return;
    }

    int choice = arrayRead(choices, iterator);
    if (choice > BOOLEAN_SEPARATOR) {
      buf++;
      R = R + 1;
    }
    while (iterator > 0) {
      buf++;
      R = R + 1;
      choice = arrayRead(choices, iterator);
      if (choice > BOOLEAN_SEPARATOR) {
        buf++;
        R = R + 1;
      }
      iterator--;
    }
  }
}
