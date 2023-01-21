package brbo.benchmarks.sas22.stac;

import brbo.benchmarks.Common;

public abstract class TemplateEngine extends Common {
  void execute(int[] text) {
    if (arrayLength(text) <= 1) {
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
    while (i + 1 < arrayLength(text)) {
      tag = arrayRead(text, i);
      R = R + tag;
      chunk = arrayRead(text, i + 1);
      R = R + chunk;
      i += 2;
    }
  }
}
