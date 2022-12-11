package brbo.benchmarks.sas22.sas21.stac;

import brbo.benchmarks.Common;

public abstract class TemplateEngine extends Common {
  void main(int[] text) {
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
    while (i + 1 < arrayLength(text)) {
      tag = arrayRead(text, i);
      chunk = arrayRead(text, i + 1);
      R = R + chunk;
      i++;
    }
  }
}
