package brbo.benchmarks.sas22.archive;

import brbo.benchmarks.Common;

public abstract class Test03 extends Common {
  void main(int n) {
    int R = 0;
    // boundAssertion("most", R <= 151);

    R = R + 1;
    R = R + 50;
    R = R + 100;
  }
}
// Test the imprecision caused by the variations in the costs of segments