abstract class TemplateEngine2 {
  void execute(int text, int templateds, int separator) 
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
    lessPreciseBound((((0 + (D0p * C0)) + (D1p * C1)) < (((((((((((8 * text) * templateds) + ((8 * separator) * templateds)) + ((8 * separator) * text)) + ((8 * text) * text)) + ((8 * separator) * separator)) + ((8 * templateds) * templateds)) + (8 * text)) + (8 * templateds)) + (8 * separator)) + 8)) || (((0 + (D0p * C0)) + (D1p * C1)) == (((((((((((8 * text) * templateds) + ((8 * separator) * templateds)) + ((8 * separator) * text)) + ((8 * text) * text)) + ((8 * separator) * separator)) + ((8 * templateds) * templateds)) + (8 * text)) + (8 * templateds)) + (8 * separator)) + 8)));
    mostPreciseBound((((0 + (D0p * C0)) + (D1p * C1)) < ((text * templateds) + (separator * templateds))) || (((0 + (D0p * C0)) + (D1p * C1)) == ((text * templateds) + (separator * templateds))));
    if ((((text < 1) || (text == 1)) || ((templateds < 0) || (templateds == 0))) || ((separator < 0) || (separator == 0)))
    {
      return;
    }
    else
    {
      ;
    }
    int R = 0;
    // mostPreciseBound(R <= arraySum(text) * templateds + separator * templateds)
    // lessPreciseBound(R <= MAX * arraySum(text) * templateds + MAX * separator * templateds + MAX * separator * arraySum(text) + MAX * arraySum(text) * arraySum(text) + MAX * separator * separator + MAX * templateds * templateds + MAX * arraySum(text) + MAX * templateds + MAX * separator + MAX)
    int i = 0;
    int j = 0;
    int chunk = 0;
    int tag = 0;
    while (i < templateds)
    {
      j = 0;
      {
        temporaryText = ndInt2(lastIndexOfText, text);
        chunk = temporaryText - lastIndexOfText;
        lastIndexOfText = temporaryText;
      }
      D1 = D1 + chunk;
      j = j + 1;
      while ((j + 1) < text)
      {
        {
          temporaryText = ndInt2(lastIndexOfText, text);
          tag = temporaryText - lastIndexOfText;
          lastIndexOfText = temporaryText;
        }
        {
          temporaryText = ndInt2(lastIndexOfText, text);
          chunk = temporaryText - lastIndexOfText;
          lastIndexOfText = temporaryText;
        }
        D1 = D1 + chunk;
        j = j + 2;
        // reset R1
        // reset R0
      }
      i = i + 1;
      D0 = D0 + separator;
      D1p = D1;
      D1 = 0;
      C1 = C1 + 1;
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