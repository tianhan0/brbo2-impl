abstract class GetCanonicalName {
  void execute(int className) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int C0 = -1;
    int D0 = 0;
    int D0p = 0;
    lessPreciseBound(((0 + (D0p * C0)) < ((8 * className) + 8)) || ((0 + (D0p * C0)) == ((8 * className) + 8)));
    mostPreciseBound(((0 + (D0p * C0)) < className) || ((0 + (D0p * C0)) == className));
    if ((className < 0) || (className == 0))
    {
      return;
    }
    else
    {
      ;
    }
    int R = 0;
    // mostPreciseBound(R <= className)
    // lessPreciseBound(R <= MAX * className + MAX)
    int i = 0;
    while (i < className)
    {
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