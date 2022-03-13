package brbo.benchmarks.sas21.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class AppendDisplayNames extends Common {
  void main(int sorted) {
    if (sorted <= 0) {
      return;
    }
    int sb = 0;
    int R = 0;
    boundAssertion("most", R <= sorted + 2 * sorted);
    boundAssertion("less", R <= MAX * sorted + MAX);
    int iterator = sorted;
    while (iterator > 0) {
      int entry = uninitialized();
      ndInt3(1, entry, iterator);
      iterator -= entry;

      for (int i = 0; i < entry; i++) {
        if (ndInt() == 0) {
          sb += 2;
          R = R + 2;
        } else {
          sb++;
          R = R + 1;
        }
      }
      if (ndInt() == 0) {
        sb++;
        R = R + 1;
      }
    }
  }
}
