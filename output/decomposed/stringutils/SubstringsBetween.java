abstract class SubstringsBetween {
  void execute(int str, int open, int close) 
  {
    int BOOLEAN_SEPARATOR = 500;
    int C0 = -1;
    int D0 = 0;
    int D0p = 0;
    lessPreciseBound(((0 + (D0p * C0)) < ((8 * str) + 8)) || ((0 + (D0p * C0)) == ((8 * str) + 8)));
    mostPreciseBound(((0 + (D0p * C0)) < str) || ((0 + (D0p * C0)) == str));
    if ((((str < 0) || (str == 0)) || ((open < 0) || (open == 0))) || ((close < 0) || (close == 0)))
    {
      return;
    }
    else
    {
      ;
    }
    int R = 0;
    // mostPreciseBound(R <= str)
    // lessPreciseBound(R <= MAX * str + MAX)
    int list = 0;
    int pos = 0;
    while (pos < (str - close))
    {
      int start = ndInt2(pos, str);
      if (start == str)
      {
        break;
      }
      else
      {
        ;
      }
      start = start + open;
      int end = ndInt2(start, str);
      if (end == str)
      {
        break;
      }
      else
      {
        ;
      }
      list = list + (end - start);
      D0 = D0 + (end - start);
      pos = end + close;
      // resetPlaceHolder_1();
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