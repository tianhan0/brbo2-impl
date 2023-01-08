package brbo.benchmarks.sas22.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class SplitWorker2 extends Common {
  void execute(int str,
               int separatorChars,
               int max,
               boolean preserveAllTokens,
               int[] choices) {
    if (str <= 0 || max <= 0 || separatorChars < 0 ||
        arrayLength(choices) < str) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= str);
    lessPreciseBound(R <= MAX * str + MAX);
    int list = 0;
    int sizePlus1 = 1;
    int i = 0;
    int start = 0;
    boolean match = false;
    boolean lastMatch = false;
    int choice = 0;
    if (separatorChars == 0) {
      while (i < str) {
        choice = arrayRead(choices, i);
        if (choice > BOOLEAN_SEPARATOR) {
          if (match || preserveAllTokens) {
            lastMatch = true;
            if (sizePlus1 == max) {
              i = str;
              lastMatch = false;
            }
            sizePlus1++;
            list += i - start;
            R = R + (i - start);
            match = false;
          }
          i++;
          start = i;
          continue;
        }
        lastMatch = false;
        match = true;
        i++;
      }
    } else {
      if (separatorChars == 1) {
        while (i < str) {
          choice = arrayRead(choices, i);
          if (choice > BOOLEAN_SEPARATOR) {
            if (match || preserveAllTokens) {
              lastMatch = true;
              if (sizePlus1 == max) {
                i = str;
                lastMatch = false;
              }
              sizePlus1++;
              list += i - start;
              R = R + (i - start);
              match = false;
            }
            i++;
            start = i;
            continue;
          }
          lastMatch = false;
          match = true;
          i++;
        }
      } else {
        while (i < str) {
          choice = arrayRead(choices, i);
          if (choice > BOOLEAN_SEPARATOR) {
            if (match || preserveAllTokens) {
              lastMatch = true;
              if (sizePlus1 == max) {
                i = str;
                lastMatch = false;
              }
              sizePlus1++;
              list += i - start;
              R = R + (i - start);
              match = false;
            }
            i++;
            start = i;
            continue;
          }
          lastMatch = false;
          match = true;
          i++;
        }
      }
    }
    if (match || preserveAllTokens && lastMatch) {
      list += i - start;
      R = R + (i - start);
    }
  }
}
