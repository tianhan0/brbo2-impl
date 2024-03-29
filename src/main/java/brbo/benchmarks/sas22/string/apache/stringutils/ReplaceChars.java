package brbo.benchmarks.sas22.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class ReplaceChars extends Common {
  void execute(int str, int replaceChars) {
    if (str <= 0 || replaceChars <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= str);
    lessPreciseBound(R <= MAX * str + MAX);
    int choice = ndInt2(0, str - 1);
    for (int i = 0; i < str; i++) {
      if (i >= choice) {
        // Replace
        if (i < replaceChars) {
          R = R + 1;
        }
      } else {
        // Not replace
        R = R + 1;
      }
    }
  }
}
