abstract class AppendDetail {
  void execute(int array) 
  {
    int BOOLEAN_SEPARATOR = 502;
    int C0 = -1;
    int D0 = 0;
    int D0p = 0;
    lessPreciseBound(((0 + (D0p * C0)) < (8 + (8 * array))) || ((0 + (D0p * C0)) == (8 + (8 * array))));
    mostPreciseBound(((0 + (D0p * C0)) < (1 + array)) || ((0 + (D0p * C0)) == (1 + array)));
    if ((array < 0) || (array == 0))
    {
      return;
    }
    else
    {
      ;
    }
    int buffer = 0;
    int R = 0;
    // mostPreciseBound(R <= 1 + array)
    // lessPreciseBound(R <= MAX + MAX * array)
    int i = 0;
    while (i < array)
    {
      if (!((i < 0)) && !((i == 0)))
      {
        buffer = buffer + 1;
        D0 = D0 + 1;
      }
      else
      {
        ;
      }
      buffer = buffer + 1;
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