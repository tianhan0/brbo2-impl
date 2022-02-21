package brbo.benchmarks.sas21.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class SplitByWholeSeparatorWorker extends Common {
  void main(int str, int separator, int max, boolean preserveAllTokens) {
    if (str <= 0 || separator <= 0 || max <= 0) {
      return;
    }
    int R = 0;
    boundAssertion("most", R <= str);
    boundAssertion("less", R <= MAX * str + MAX);
    int substrings = 0;
    int numberOfStrings = 0;
    int beg = 0;
    int end = 0;
    while (end < str) {
      if (ndBool()) {
        end = -1;
      }
      else {
        end = ndInt2(beg, str - 1);
      }
      if (end > -1) {
        if (end > beg) {
          numberOfStrings++;
          if (numberOfStrings == max) {
            end = str;
            substrings += str - beg;
            R = R + (str - beg);
          }
          else {
            substrings += end - beg;
            R = R + (end - beg);
            beg = end + separator;
          }
        }
        else {
          if (preserveAllTokens) {
            numberOfStrings++;
            if (numberOfStrings == max) {
              end = str;
              substrings += str - beg;
              R = R + (str - beg);
            }
            else {
              ;
            }
          }
          beg = end + separator;
        }
      }
      else {
        substrings += str - beg;
        R = R + (str - beg);
        end = str;
      }
    }
  }
}
