abstract class Replace {
  void execute(int text, int searchString, int replacement, int max) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int C0 = -1;
    int C1 = -1;
    int D0 = 0;
    int D0p = 0;
    int D1 = 0;
    int D1p = 0;
    int temporaryText = 0;
    int lastIndexOfText = 0;
    lessPreciseBound((((0 + (D0 + (D0p * (C0 - 1)))) + (D1 + (D1p * (C1 - 1)))) < ((8 + (8 * text)) + ((8 * text) * replacement))) || (((0 + (D0 + (D0p * (C0 - 1)))) + (D1 + (D1p * (C1 - 1)))) == ((8 + (8 * text)) + ((8 * text) * replacement))));
    mostPreciseBound((((0 + (D0 + (D0p * (C0 - 1)))) + (D1 + (D1p * (C1 - 1)))) < (text + (text * replacement))) || (((0 + (D0 + (D0p * (C0 - 1)))) + (D1 + (D1p * (C1 - 1)))) == (text + (text * replacement))));
    if (((((text < 1) || (text == 1)) || ((searchString < 0) || (searchString == 0))) || ((max < 0) || (max == 0))) || ((replacement < 0) || (replacement == 0)))
    {
      return;
    }
    else
    {
      ;
    }
    // int R = 0;
    // mostPreciseBound(R <= arraySum(text) + arraySum(text) * replacement)
    // lessPreciseBound(R <= MAX + MAX * arraySum(text) + MAX * arraySum(text) * replacement)
    int chunk = 0;
    int i = 0;
    while ((i + 1) < text)
    {
      {
        temporaryText = ndInt2(lastIndexOfText, text);
        chunk = temporaryText - lastIndexOfText;
        lastIndexOfText = temporaryText;
      }
      D1 = D1 + chunk;
      D0 = D0 + replacement;
      {
        temporaryText = ndInt2(lastIndexOfText, text);
        chunk = temporaryText - lastIndexOfText;
        lastIndexOfText = temporaryText;
      }
      i = i + 2;
      // reset R1
      D0p = D0;
      D0 = 0;
      C0 = C0 + 1;
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