package brbo.benchmarks.sas22.sas21.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class LongToHex extends Common {
  void execute(int nHexs) {
    if (nHexs <= 0) {
      return;
    }
    int sb = 0;
    int R = 0;
    mostPreciseBound(R <= nHexs);
    lessPreciseBound(R <= MAX * nHexs + MAX);
    for (int i = 0;i < nHexs; i++) {
      if (ndBool()) {
        sb++;
        R = R + 1;
      }
    }
  }
}
