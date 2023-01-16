abstract class SubstringsBetween {
  void execute(int str) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int R = 0;
    lessPreciseBound((R < ((8 * str) + 8)) || (R == ((8 * str) + 8)));
    mostPreciseBound((R < str) || (R == str));
    if ((str < 0) || (str == 0))
    {
      return;
    }
    else
    {
      ;
    }
    // int R = 0;
    // mostPreciseBound(R <= str)
    // lessPreciseBound(R <= MAX * str + MAX)
    int list = 0;
    int pos = 0;
    int open = 1;
    int close = ndInt2(0, str - 1);
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
      R = R + (end - start);
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