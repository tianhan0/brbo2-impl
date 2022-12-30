package brbo.benchmarks.sas22.stac;

import brbo.benchmarks.Common;

public abstract class TemplateEngineSimplified extends Common {
  void execute(int[] text) {
    if (arraySum(text) <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= arraySum(text));
    lessPreciseBound(R <= MAX * arraySum(text) + MAX);

    int chunk = 0;
    int tag = 0;
    int i = 0;
    chunk = arrayRead(text, i);
    R = R + chunk;
    i++;
    while (i < arrayLength(text)) {
      chunk = arrayRead(text, i);
      R = R + chunk;
      i += 1;
    }
  }
}
