package brbo.benchmarks.sas22.stac;

import brbo.benchmarks.Common;

public abstract class TemplateEngine2 extends Common {
  void execute(int[] text, int templateds, int separator) {
    if (arrayLength(text) <= 1 || templateds <= 0 || separator <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= arraySum(text) * templateds + separator * templateds);
    lessPreciseBound(R <= MAX * arraySum(text) * templateds +
        MAX * separator * templateds +
        MAX * separator * arraySum(text) +
        MAX * arraySum(text) * arraySum(text) +
        MAX * separator * separator +
        MAX * templateds * templateds +
        MAX * arraySum(text) +
        MAX * templateds + MAX * separator +
        MAX
    );

    int i = 0;
    int j = 0;
    int chunk = 0;
    int tag = 0;
    while (i < templateds) {
      j = 0;
      chunk = arrayRead(text, j);
      R = R + chunk;
      j++;
      while (j + 1 < arrayLength(text)) {
        tag = arrayRead(text, j);
        R = R + tag;
        chunk = arrayRead(text, j + 1);
        R = R + chunk;
        j += 2;
      }
      i++;
      R = R + separator;
    }
  }
}
