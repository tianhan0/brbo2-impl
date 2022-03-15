package brbo.benchmarks.sas21.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class AppendAllTo extends Common {
  void main(int sep, int types) {
    if (sep <= 0 || types <= 0) {
      return;
    }
    int builder = 0;
    int R = 0;
    boundAssertion("most", R <= types * sep + types + 1);
    boundAssertion("less", R <= MAX * types * types +
        MAX * sep * sep +
        MAX * types * sep +
        MAX * types + MAX * sep +
        MAX
    );
    builder++;
    R = R + 1;
    for (int i = 1; i < types; i++) {
      builder += sep;
      R = R + sep;
      builder++;
      R = R + 1;
    }
  }
}
// We must remove all resets at line 25 from the initial amortization.
// The removal may require an infinite number of refinement steps, if each step removes a single reset.