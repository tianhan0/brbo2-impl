package brbo.benchmarks.sas21.containers.stac;

import brbo.benchmarks.Common;

public abstract class TemplateEngine2 extends Common {
  void main(int text, int templateds, int separator) {
    if (text <= 0 || templateds <= 0 || separator <= 0
            || text > LARGE_INT || templateds > LARGE_INT || separator > LARGE_INT) {
      return;
    }
    int R = 0;
    boundAssertion("most", R <= text * templateds + separator * templateds);
    boundAssertion("less", R <= MAX * text * templateds +
        MAX * separator * templateds +
        MAX * separator * text +
        MAX * text * text +
        MAX * separator * separator +
        MAX * templateds * templateds +
        MAX * text +
        MAX * templateds + MAX * separator +
        MAX
    );

    int i = 0;
    while (i < templateds) {
      int linePointer = 0;
      int startTagLocation = 0;
      int endTagLocation = 0;
      int stringBuilder = 0;
      while (endTagLocation <= text) {
        ndInt3(endTagLocation, startTagLocation, text);
        ndInt3(startTagLocation + 1, endTagLocation, text);
        stringBuilder += startTagLocation - linePointer;
        R = R + (startTagLocation - linePointer);
        linePointer = endTagLocation;
      }
      stringBuilder += text - linePointer;
      R = R + (text - linePointer);
      i++;
      R = R + separator;
    }
  }
}
