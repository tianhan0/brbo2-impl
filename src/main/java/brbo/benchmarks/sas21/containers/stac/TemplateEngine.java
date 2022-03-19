package brbo.benchmarks.sas21.containers.stac;

import brbo.benchmarks.Common;

public abstract class TemplateEngine extends Common {
  void main(int text) {
    if (text <= 0) {
      return;
    }
    int R = 0;
    boundAssertion("most", R <= text);
    boundAssertion("less", R <= MAX * text + MAX);

    int linePointer = 0;
    int startTagLocation = 0;
    int endTagLocation = 0;
    // int stringBuilder = 0;
    while (endTagLocation < text) {
      startTagLocation = ndInt();
      ndInt3(endTagLocation, startTagLocation, text);
      endTagLocation = ndInt();
      ndInt3(startTagLocation, endTagLocation, text);
      // stringBuilder += startTagLocation - linePointer;
      R = R + (startTagLocation - linePointer);
      linePointer = endTagLocation;
    }
    // stringBuilder += text - linePointer;
    R = R + (text - linePointer);
  }
  // No (Imprecise); Yes; Yes (Same as Brbo)
}
