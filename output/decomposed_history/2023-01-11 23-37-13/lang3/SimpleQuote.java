abstract class SimpleQuote {
  void execute(int value) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int C0 = -1;
    int C1 = -1;
    int D0 = 0;
    int D0p = 0;
    int D1 = 0;
    int D1p = 0;
    lessPreciseBound((((0 + (D0 + (D0p * (C0 - 1)))) + (D1 + (D1p * (C1 - 1)))) < ((8 * value) + 8)) || (((0 + (D0 + (D0p * (C0 - 1)))) + (D1 + (D1p * (C1 - 1)))) == ((8 * value) + 8)));
    mostPreciseBound((((0 + (D0 + (D0p * (C0 - 1)))) + (D1 + (D1p * (C1 - 1)))) < ((value * 2) + 1)) || (((0 + (D0 + (D0p * (C0 - 1)))) + (D1 + (D1p * (C1 - 1)))) == ((value * 2) + 1)));
    if ((value < 0) || (value == 0))
    {
      return;
    }
    else
    {
      ;
    }
    int sb = 0;
    int R = 0;
    // mostPreciseBound(R <= value * 2 + 1)
    // lessPreciseBound(R <= MAX * value + MAX)
    int i = 0;
    while (i < value)
    {
      sb = sb + 2;
      D0 = D0 + 2;
      i = i + 1;
      // reset R1
      D0p = D0;
      D0 = 0;
      C0 = C0 + 1;
    }
    sb = sb + 1;
    D1 = D1 + 1;
  }
  // Declare these methods such that these generated code can be parsed
  public abstract int ndInt();
  public abstract int ndInt2(int lower, int upper);
  public abstract boolean ndBool();
  public abstract void assume(boolean expression);
  public abstract void mostPreciseBound(boolean assertion);
  public abstract void lessPreciseBound(boolean assertion);
}