abstract class TemplateEngine {
  void execute(int text) 
  {
    int C0 = -1;
    int D0 = 0;
    int D0p = 0;
    int temporaryText = 0;
    int lastIndexOfText = 0;
    lessPreciseBound(((0 + D0) < ((8 * text) + 8)) || ((0 + D0) == ((8 * text) + 8)));
    mostPreciseBound(((0 + D0) < text) || ((0 + D0) == text));
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
      temporaryText = ndInt2(lastIndexOfText, chunk);
      lastIndexOfText = lastIndexOfText + temporaryText;
      chunk = temporaryText;
    }
    D0 = D0 + chunk;
    i = i + 1;
    while ((i + 1) < arrayLength(text))
    {
      {
        temporaryText = ndInt2(lastIndexOfText, tag);
        lastIndexOfText = lastIndexOfText + temporaryText;
        tag = temporaryText;
      }
      {
        temporaryText = ndInt2(lastIndexOfText, chunk);
        lastIndexOfText = lastIndexOfText + temporaryText;
        chunk = temporaryText;
      }
      D0 = D0 + chunk;
      i = i + 2;
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
  public abstract int arrayLength(int array);
}