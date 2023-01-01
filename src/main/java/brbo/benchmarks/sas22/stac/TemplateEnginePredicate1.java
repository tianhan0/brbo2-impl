package brbo.benchmarks.sas22.stac;

import brbo.benchmarks.Common;

public abstract class TemplateEnginePredicate1 extends Common {
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
    while (i < arrayLength(text)) {
      chunk = arrayRead(text, i);
      R = R + chunk;
      if (i + 1 < arrayLength(text)) {
        tag = arrayRead(text, i + 1);
        if (i == 1) {
          R = R + tag;
        }
        i += 2;
      } else {
        i++;
      }
    }
  }
}
