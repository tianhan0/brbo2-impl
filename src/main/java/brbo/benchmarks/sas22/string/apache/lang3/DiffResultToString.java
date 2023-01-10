package brbo.benchmarks.sas22.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class DiffResultToString extends Common {
  void execute(int[] diff) {
    if (arrayLength(diff) <= 1) {
      return;
    }
    int lhsBuilder = 0;
    int rhsBuilder = 0;
    int R = 0;
    mostPreciseBound(R <= arraySum(diff));
    lessPreciseBound(R <= MAX * arraySum(diff)+ MAX);
    int chunk = 0;
    for (int i = 0; i < arrayLength(diff); i++) {
      chunk = arrayRead(diff, i);
      R = R + chunk;
    }
    /*
      int fieldName = ndInt2(1, entry);
      if (entry - fieldName < 1)
        break;
      int left = ndInt2(1, entry - fieldName);
      if (entry - fieldName - left < 1)
        break;
      int right = ndInt2(1, entry - fieldName - left);
      lhsBuilder += fieldName + left;
      R = R + (fieldName + left);
      rhsBuilder += fieldName + right;
      R = R + (fieldName + right);*/
  }
}
