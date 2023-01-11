package brbo.benchmarks.sas22.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class SplitWorker extends Common {
  void execute(int str, int choice) {
    if (str <= 0 || !(choice >= 0 && choice < str)) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= str);
    lessPreciseBound(R <= MAX * str + MAX);
    int list = 0;
    int i = 0;
    int start = 0;
    boolean match = false;
    boolean lastMatch = false;
    while (i < str) {
      if (i > choice) {
        if (match) {
          list += i - start;
          R = R + (i - start);
          match = false;
          lastMatch = true;
        }
        ++i;
        start = i;
        continue;
      }
      lastMatch = false;
      match = true;
      i++;
    }
    if (match && lastMatch) {
      list += i - start;
      R = R + (i - start);
    }
  }
}
