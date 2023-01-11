abstract class PadEnd {
  void execute(int string, int minLength) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int C0 = -1;
    int C1 = -1;
    int D0 = 0;
    int D0p = 0;
    int D1 = 0;
    int D1p = 0;
    lessPreciseBound((((0 + (D0p * C0)) + (D1p * C1)) < (((8 * minLength) + (8 * string)) + 8)) || (((0 + (D0p * C0)) + (D1p * C1)) == (((8 * minLength) + (8 * string)) + 8)));
    mostPreciseBound((((0 + (D0p * C0)) + (D1p * C1)) < minLength) || (((0 + (D0p * C0)) + (D1p * C1)) == minLength));
    if (((string < 0) || (string == 0)) || ((minLength < 0) || (minLength == 0)))
    {
      return;
    }
    else
    {
      ;
    }
    int sb = 0;
    int R = 0;
    // mostPreciseBound(R <= minLength)
    // lessPreciseBound(R <= MAX * minLength + MAX * string + MAX)
    sb = sb + string;
    D1 = D1 + string;
    int i = string;
    while (i < minLength)
    {
      sb = sb + 1;
      D0 = D0 + 1;
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