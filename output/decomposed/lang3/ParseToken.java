abstract class ParseToken {
  void execute(int pattern, int i) 
  {
    int BOOLEAN_SEPARATOR = 502;
    int C0 = -1;
    int D0 = 0;
    int D0p = 0;
    lessPreciseBound(((0 + (D0p * C0)) < ((8 * pattern) + 8)) || ((0 + (D0p * C0)) == ((8 * pattern) + 8)));
    mostPreciseBound(((0 + (D0p * C0)) < (pattern + 1)) || ((0 + (D0p * C0)) == (pattern + 1)));
    if ((((pattern < 0) || (pattern == 0)) || !((i < pattern))) || ((i < 0) || (i == 0)))
    {
      return;
    }
    else
    {
      ;
    }
    int buf = 0;
    int R = 0;
    // mostPreciseBound(R <= pattern + 1)
    // lessPreciseBound(R <= MAX * pattern + MAX)
    if (true)
    {
      buf = buf + 1;
      D0 = D0 + 1;
      while ((i + 1) < pattern)
      {
        buf = buf + 1;
        D0 = D0 + 1;
        i = i + 1;
        // reset R0
      }
    }
    else
    {
      buf = buf + 1;
      R = R + 1;
      while (i < pattern)
      {
        i = i + 1;
        buf = buf + 1;
        R = R + 1;
        i = i + 1;
        // resetPlaceHolder_2();
      }
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