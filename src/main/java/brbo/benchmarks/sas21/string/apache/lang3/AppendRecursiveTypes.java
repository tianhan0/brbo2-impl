package brbo.benchmarks.sas21.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class AppendRecursiveTypes extends Common {
  void main(int argumentTypes, int recursiveTypeIndexes) {
    if (argumentTypes <= 0 || recursiveTypeIndexes <= 0) {
      return;
    }
    int builder = 0;
    int R = 0;
    boundAssertion("most", R <= recursiveTypeIndexes + 1 + argumentTypes * 3);
    boundAssertion("less", R <= MAX * recursiveTypeIndexes + MAX * argumentTypes + MAX);
    for (int i = 0; i < recursiveTypeIndexes; i++) {
      int sep = 2;

      builder++;
      R = R + 1;
      for (int i2 = 1; i2 < 1; i2++) {
        builder += sep;
        R = R + sep;
        builder++;
        R = R + 1;
      }
    }

    int d = ndInt();
    ndInt3(0, d, recursiveTypeIndexes);
    int argumentsFiltered = argumentTypes - d;

    if (argumentsFiltered > 0) {
      builder++;
      R = R + 1;
      int sep2 = 2;
      for (int i3 = 1; i3 < argumentsFiltered; i3++) {
        builder += sep2;
        R = R + sep2;
        builder++;
        R = R + 1;
      }
    }
  }
}
