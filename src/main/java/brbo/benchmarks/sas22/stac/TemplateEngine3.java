package brbo.benchmarks.sas22.stac;

import brbo.benchmarks.Common;

public abstract class TemplateEngine3 extends Common {
  void execute(int[] text, int ts, int sep, int rep) {
    if (arrayLength(text) <= 1 || ts <= 0 || sep <= 0 || rep <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= ts * (arraySum(text) + arraySum(text) * rep + sep));
    lessPreciseBound(R <= MAX * arraySum(text) * ts +
        MAX * arraySum(text) * sep +
        MAX * arraySum(text) * arraySum(text) +
        MAX * arraySum(text) * rep +
        MAX * ts * sep +
        MAX * ts * arraySum(text) +
        MAX * ts * rep +
        MAX * sep * arraySum(text) +
        MAX * sep * rep +
        MAX * arraySum(text) * rep +
        MAX * arraySum(text) +
        MAX * ts +
        MAX * sep +
        MAX * arraySum(text) +
        MAX * rep +
        MAX
    );

    int chunk = 0;
    for (int i = 0; i < ts; i++) {
      for (int j = 0; j < arrayLength(text); j++) {
        chunk = arrayRead(text, j);
        R = R + chunk;
        R = R + rep;
      }
      R = R + sep;
    }
  }
}
