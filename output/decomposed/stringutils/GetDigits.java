abstract class GetDigits {
  void execute(int str, int choices) 
  {
    int BOOLEAN_SEPARATOR = 502;
    int C0 = -1;
    int D0 = 0;
    int D0p = 0;
    int temporaryChoices = 0;
    int lastIndexOfChoices = 0;
    lessPreciseBound(((0 + (D0 + (D0p * C0))) < (8 * str)) || ((0 + (D0 + (D0p * C0))) == (8 * str)));
    mostPreciseBound(((0 + (D0 + (D0p * C0))) < str) || ((0 + (D0 + (D0p * C0))) == str));
    if (((str < 0) || (str == 0)) || (choices < str))
    {
      return;
    }
    else
    {
      ;
    }
    int R = 0;
    // mostPreciseBound(R <= str)
    // lessPreciseBound(R <= MAX * str)
    int strDigits = 0;
    int choice = 0;
    int i = 0;
    while (i < str)
    {
      {
        temporaryChoices = ndInt2(lastIndexOfChoices, choices);
        choice = temporaryChoices - lastIndexOfChoices;
        lastIndexOfChoices = temporaryChoices;
      }
      if (!((choice < 500)) && !((choice == 500)))
      {
        strDigits = strDigits + 1;
        D0 = D0 + 1;
      }
      else
      {
        ;
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