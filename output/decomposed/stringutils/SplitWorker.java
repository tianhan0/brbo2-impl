abstract class SplitWorker {
  void execute(int str, int choice) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int C0 = -1;
    int D0 = 0;
    int D0p = 0;
    lessPreciseBound(((0 + (D0p * C0)) < ((8 * str) + 8)) || ((0 + (D0p * C0)) == ((8 * str) + 8)));
    mostPreciseBound(((0 + (D0p * C0)) < str) || ((0 + (D0p * C0)) == str));
    if (((str < 0) || (str == 0)) || !((!((choice < 0)) && (choice < str))))
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
    int i = 0;
    int start = 0;
    boolean match = false;
    boolean lastMatch = false;
    while (i < str)
    {
      if (!((i < choice)) && !((i == choice)))
      {
        if (match)
        {
          list = list + (i - start);
          D0 = D0 + (i - start);
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
      // reset R0
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