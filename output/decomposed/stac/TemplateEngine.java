abstract class TemplateEngine {
  void main(int text) 

  {
    int C0 = -1;
    int D0 = 0;
    int D0p = 0;
    lessPreciseBound(((0 + D0) < ((8 * arraySum(text)) + 8)) || ((0 + D0) == ((8 * arraySum(text)) + 8)));
    mostPreciseBound(((0 + D0) < arraySum(text)) || ((0 + D0) == arraySum(text)));
    if ((arraySum(text) < 0) || (arraySum(text) == 0))
    {
      return;
    }
    else
    {

    }
    int R = 0;
    // mostPreciseBound(R <= arraySum(text))
    // lessPreciseBound(R <= MAX * arraySum(text) + MAX)
    int chunk = 0;
    int tag = 0;
    int i = 0;
    chunk = arrayRead(text, i);
    D0 = D0 + chunk;
    i = i + 1;
    while ((i + 1) < arrayLength(text))
    {
      // reset R0
      {
        tag = arrayRead(text, i);
        chunk = arrayRead(text, (i + 1));
        D0 = D0 + chunk;
        i = i + 2;
      }
    }
  }
  public abstract int ndInt();
  public abstract int ndInt2(int lower, int upper);
  public abstract boolean ndBool();
  public abstract void assume(boolean expression);
  public abstract void mostPreciseBound(boolean assertion);
  public abstract void lessPreciseBound(boolean assertion);
  public abstract void resetPlaceHolder();
}