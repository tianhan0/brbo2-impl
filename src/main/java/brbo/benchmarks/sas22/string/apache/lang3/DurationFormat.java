package brbo.benchmarks.sas22.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class DurationFormat extends Common {
  void execute(int[] tokens, int years, int months) {
    if (arrayLength(tokens) <= 1 || years <= 0 || months <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= arraySum(tokens) + arraySum(tokens) * (years + months));
    lessPreciseBound(R <= MAX * arraySum(tokens) * arraySum(tokens) +
        MAX * years * years + MAX * months * months +
        MAX * arraySum(tokens) * years + MAX * arraySum(tokens) * months +
        MAX * months + MAX * years + MAX * arraySum(tokens) + MAX
    );
    int chunk = 0;
    for (int i = 0; i < arrayLength(tokens); i++) {
      chunk = arrayRead(tokens, i);
      R = R + chunk;
      R = R + years;
      R = R + months;
    }
  }
}
