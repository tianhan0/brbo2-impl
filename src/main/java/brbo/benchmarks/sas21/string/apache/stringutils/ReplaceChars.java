package brbo.benchmarks.sas21.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class ReplaceChars extends Common {
  void main(int str, int searchChars, int replaceChars) {
    if (str <= 0 || searchChars <= 0 || replaceChars <= 0) {
      return;
    }
    boolean modified = false;
    int buf = 0;
    int R = 0;
    boundAssertion("most", R <= str);
    boundAssertion("less", R <= MAX * str + MAX);
    for (int i = 0; i < str; i++) {
      int index = 0;
      if (ndInt() == 0) {
        index = -1;
      }
      else {
        ndInt3(0, index, searchChars - 1);
      }
      if (index >= 0) {
        modified = true;
        if (index < replaceChars) {
          buf++;
          R = R + 1;
        }
      } else {
        buf++;
        R = R + 1;
      }
    }
  }
}
