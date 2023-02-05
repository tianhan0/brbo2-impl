package brbo.benchmarks.sas22.stac;

import brbo.benchmarks.Common;

public abstract class TemplateEngineMultiTrace extends Common {
  void execute(int[] text, int choice) {
    if (arrayLength(text) <= 1) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= arraySum(text));
    lessPreciseBound(R <= MAX * arraySum(text) + MAX);

    int chunk = 0;
    if (choice > BOOLEAN_SEPARATOR) {
      for (int i = 0; i < arrayLength(text); i++) {
        chunk = arrayRead(text, i);
        R = R + chunk;
      }
    } else {
      for (int j = 0; j < arrayLength(text); j++) {
        chunk = arrayRead(text, j);
        R = R + chunk;
      }
    }
  }
}
