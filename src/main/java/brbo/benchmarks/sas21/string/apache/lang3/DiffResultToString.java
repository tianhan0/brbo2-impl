package brbo.benchmarks.sas21.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class DiffResultToString extends Common {
  void main(int diff) {
    if (diff <= 0) {
      return;
    }
    int lhsBuilder = 0;
    int rhsBuilder = 0;
    int R = 0;
    boundAssertion("most", R <= diff);
    boundAssertion("less", R <= MAX * diff+ MAX);
    int iterator = diff;
    while (iterator > 0) {
      int entry = ndInt();
      ndInt3(1, entry, iterator);
      iterator -= entry;
      int fieldName = ndInt();
      ndInt3(1, fieldName, entry);
      int left = ndInt();
      ndInt3(1, left, entry - fieldName);
      int right = ndInt();
      ndInt3(1, right, entry - fieldName - left);
      lhsBuilder += fieldName + left;
      R = R + (fieldName + left);
      rhsBuilder += fieldName + right;
      R = R + (fieldName + right);
    }
  }
}
