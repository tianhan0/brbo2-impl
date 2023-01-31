package brbo.benchmarks.sas22.stac;

import brbo.benchmarks.Common;

public abstract class TemplateEngineSimplifiedVariant extends Common {
  void execute(int[] text) {
    if (arrayLength(text) <= 1) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= arraySum(text) + arraySum(text) * 10 + arraySum(text) * 30);
    lessPreciseBound(R <= MAX * arraySum(text) + arraySum(text) * 10 + arraySum(text) * 30 + MAX);

    int chunk = 0;
    for (int i = 0; i < arrayLength(text); i++) {
      chunk = arrayRead(text, i);
      R = R + chunk;
      R = R + 10;
      R = R + 30;
    }
  }
}
