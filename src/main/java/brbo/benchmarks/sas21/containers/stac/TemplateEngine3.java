package brbo.benchmarks.sas21.containers.stac;

import brbo.benchmarks.Common;

public abstract class TemplateEngine3 extends Common {
  void main(int text, int ts, int sep, int tags, int rep) {
    if (text <= 0 || ts <= 0 || sep <= 0 || tags <= 0 || rep <= 0
            || text > LARGE_INT || sep > LARGE_INT || tags > LARGE_INT || rep > LARGE_INT) {
      return;
    }
    int R = 0;
    boundAssertion("most", R <= ts * (text + tags * rep + sep));
    boundAssertion("less", R <= MAX * text * ts +
        MAX * text * sep +
        MAX * text * tags +
        MAX * text * rep +
        MAX * ts * sep +
        MAX * ts * tags +
        MAX * ts * rep +
        MAX * sep * tags +
        MAX * sep * rep +
        MAX * tags * rep+
        MAX * text +
        MAX * ts +
        MAX * sep +
        MAX * tags +
        MAX * rep +
        MAX
    );

    int i = 0;
    while (i < ts) {
      int p = 0;
      int l = 0;
      int r = 0;
      int stringBuilder = 0;
      while (r <= text) {
        ndInt3(p, l, text);
        ndInt3(l, r, text);
        stringBuilder += l - p;
        R = R + (l - p);
        stringBuilder += rep;
        R = R + rep;
        p = r;
      }
      stringBuilder += text - p;
      R = R + (text - p);
      R = R + sep;
      i++;
    }
  }
}
