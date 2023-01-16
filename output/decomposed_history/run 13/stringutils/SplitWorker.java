abstract class SplitWorker {
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
    int i = 0;
    int start = 0;
    boolean match = false;
    boolean lastMatch = false;
    int choice = ndInt2(0, str - 1);
    while (i < str)
    {
      if (!((i < choice)) && !((i == choice)))
      {
        if (match)
        {
          list = list + (i - start);
          R = R + (i - start);
          match = false;
          lastMatch = true;
        }
        else
        {
          ;
        }
        i = i + 1;
        start = i;
        continue;
      }
      else
      {
        ;
      }
      lastMatch = false;
      match = true;
      i = i + 1;
      // resetPlaceHolder_1();
    }
    if (match && lastMatch)
    {
      list = list + (i - start);
      R = R + (i - start);
    }
    else
    {
      ;
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