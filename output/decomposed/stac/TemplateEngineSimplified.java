abstract class TemplateEngineSimplified {
  void execute(int text) 
  {
    int BOOLEAN_SEPARATOR = 500;
    int C0 = -1;
    int D0 = 0;
    int D0p = 0;
    int temporaryText = 0;
    int lastIndexOfText = 0;
    lessPreciseBound(((0 + (D0 + (D0p * C0))) < ((8 * text) + 8)) || ((0 + (D0 + (D0p * C0))) == ((8 * text) + 8)));
    mostPreciseBound(((0 + (D0 + (D0p * C0))) < text) || ((0 + (D0 + (D0p * C0))) == text));
    if ((text < 0) || (text == 0))
    {
      return;
    }
    else
    {
      ;
    }
    int R = 0;
    // mostPreciseBound(R <= arraySum(text))
    // lessPreciseBound(R <= MAX * arraySum(text) + MAX)
    int chunk = 0;
    int tag = 0;
    int i = 0;
    {
      temporaryText = ndInt2(lastIndexOfText, text);
      chunk = temporaryText - lastIndexOfText;
      lastIndexOfText = temporaryText;
    }
    D0 = D0 + chunk;
    i = i + 1;
    while (i < text)
    {
      {
        temporaryText = ndInt2(lastIndexOfText, text);
        chunk = temporaryText - lastIndexOfText;
        lastIndexOfText = temporaryText;
      }
      D0 = D0 + chunk;
      i = i + 1;
      // reset R0
    }
  }
  // Declare these methods such that these generated code can be parsed
  public abstract int ndInt();
  public abstract int ndInt2(int lower, int upper);
  public abstract boolean ndBool();
  public abstract void assume(boolean expression);
  public abstract void mostPreciseBound(boolean assertion);
  public abstract void lessPreciseBound(boolean assertion);
}