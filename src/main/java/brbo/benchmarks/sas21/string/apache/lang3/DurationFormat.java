package brbo.benchmarks.sas21.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class DurationFormat extends Common {
  void main(int tokens, int years, int months) {
    if (tokens <= 0 || years <= 0 || months <= 0) {
      return;
    }
    int buffer = 0;
    int R = 0;
    boundAssertion("most", R <= tokens + tokens * (years + months));
    boundAssertion("less", R <= MAX * tokens * tokens +
        MAX * years * years + MAX * months * months +
        MAX * tokens * years + MAX * tokens * months +
        MAX * months + MAX * years + MAX * tokens + MAX
    );
    int iterator = tokens;
    while (iterator > 0) {
      int entry = uninitialized();
      ndInt3(1, entry, iterator);
      iterator -= entry;
      if (ndInt() == 0) {
        buffer += entry;
        R = R + entry;
      } else {
        if (ndInt() == 0) {
          buffer += years;
          R = R + years;
        } else {
          if (ndInt() == 0) {
            buffer += months;
            R = R + months;
          }
        }
      }
    }
  }
}
