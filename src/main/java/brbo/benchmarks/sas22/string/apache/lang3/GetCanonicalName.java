package brbo.benchmarks.sas22.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class GetCanonicalName extends Common {
  void execute(int className) {
    if (className <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= className);
    lessPreciseBound(R <= MAX * className + MAX);
    for (int i = 0; i < className; i++) {
      R = R + 1;
    }
  }
}
