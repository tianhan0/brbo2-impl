abstract class ReplaceChars {
  void execute(int str, int choice, int searchChars, int replaceChars) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int C0 = -1;
    int D0 = 0;
    int D0p = 0;
    lessPreciseBound(((0 + (D0p * C0)) < ((8 * str) + 8)) || ((0 + (D0p * C0)) == ((8 * str) + 8)));
    mostPreciseBound(((0 + (D0p * C0)) < str) || ((0 + (D0p * C0)) == str));
    if (((((str < 0) || (str == 0)) || ((searchChars < 0) || (searchChars == 0))) || ((replaceChars < 0) || (replaceChars == 0))) || !((!((choice < 0)) && (choice < str))))
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
    int i = 0;
    while (i < str)
    {
      if (!((i < choice)) && !((i == choice)))
      {
        if (i < replaceChars)
        {
          D0 = D0 + 1;
        }
        else
        {
          ;
        }
      }
      else
      {
        D0 = D0 + 1;
      }
      i = i + 1;
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