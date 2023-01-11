abstract class Squeeze {
  void execute(int str) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int C0 = -1;
    int D0 = 0;
    int D0p = 0;
    lessPreciseBound(((0 + (D0 + (D0p * (C0 - 1)))) < ((8 * str) + 8)) || ((0 + (D0 + (D0p * (C0 - 1)))) == ((8 * str) + 8)));
    mostPreciseBound(((0 + (D0 + (D0p * (C0 - 1)))) < str) || ((0 + (D0 + (D0p * (C0 - 1)))) == str));
    if ((str < 0) || (str == 0))
    {
      return;
    }
    else
    {
      ;
    }
    int buffer = 0;
    int R = 0;
    // mostPreciseBound(R <= str)
    // lessPreciseBound(R <= MAX * str + MAX)
    int i = 1;
    while (i < str)
    {
      buffer = buffer + 1;
      D0 = D0 + 1;
      i = i + 1;
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