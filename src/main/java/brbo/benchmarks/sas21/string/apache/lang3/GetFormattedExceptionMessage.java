package brbo.benchmarks.sas21.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class GetFormattedExceptionMessage extends Common {
  void main(int baseMessage, int contextValues) {
    if (baseMessage <= 0 || contextValues <= 0) {
      return;
    }
    int buffer = 0;
    int R = 0;
    boundAssertion("most", R <= 3 + 6 * contextValues);
    boundAssertion("less", R <= MAX + MAX * contextValues + MAX * baseMessage);
    buffer++;
    R  = R + 1;
    buffer++;
    R = R + 1;
    int i = 0;
    int iterator = contextValues;
    while (iterator > 0) {
      int entry = ndInt();
      ndInt3(1, entry, iterator);
      int key = ndInt();
      ndInt3(1, key, entry);
      int value = ndInt();
      ndInt3(1, value, entry - key);
      iterator -= entry;

      buffer++;
      R = R + 1;
      buffer++;
      R = R + 1;
      i++;
      buffer++;
      R = R + 1;

      buffer += key;
      R = R + key;

      buffer++;
      R = R + 1;
      if (ndInt() == 0) {
        buffer++;
        R = R + 1;
      }
      else {
        buffer += value;
        R = R + value;
      }
      buffer++;
      R = R + 1;
    }
    buffer++;
    R = R + 1;
  }
}
