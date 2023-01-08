package brbo.benchmarks.sas22.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class AppendRecursiveTypes extends Common {
  void execute(int argumentTypes, int recursiveTypeIndexes, int d) {
    if (argumentTypes <= 0 || recursiveTypeIndexes <= 0 || !(d <= recursiveTypeIndexes && d >= 0)) {
      return;
    }
    int builder = 0;
    int R = 0;
    mostPreciseBound(R <= recursiveTypeIndexes + 1 + argumentTypes * 3);
    lessPreciseBound(R <= MAX * recursiveTypeIndexes + MAX * argumentTypes + MAX);
    for (int i = 0; i < recursiveTypeIndexes; i++) {
      int sep = 2;

      builder++;
      R = R + 1;
      for (int j = 1; j < 1; j++) {
        builder += sep;
        R = R + sep;
        builder++;
        R = R + 1;
      }
    }

    int argumentsFiltered = argumentTypes - d;

    if (argumentsFiltered > 0) {
      builder++;
      R = R + 1;
      int sep2 = 2;
      for (int k = 1; k < argumentsFiltered; k++) {
        builder += sep2;
        R = R + sep2;
        builder++;
        R = R + 1;
      }
    }
  }
}
