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

    int chunk = 0;
    for (int i = 0; i < templateds; i++) {
      for (int j = 0; j < arrayLength(text); j++) {
        chunk = arrayRead(text, j);
        R = R + chunk;
      }
      R = R + separator;
    }
  }
}
