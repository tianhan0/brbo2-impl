abstract class AppendAllTo {
  void execute(int sep, int types) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int C0 = -1;
    int C1 = -1;
    int D0 = 0;
    int D0p = 0;
    int D1 = 0;
    int D1p = 0;
    lessPreciseBound((((0 + (D0p * C0)) + (D1p * C1)) < (((((((8 * types) * types) + ((8 * sep) * sep)) + ((8 * types) * sep)) + (8 * types)) + (8 * sep)) + 8)) || (((0 + (D0p * C0)) + (D1p * C1)) == (((((((8 * types) * types) + ((8 * sep) * sep)) + ((8 * types) * sep)) + (8 * types)) + (8 * sep)) + 8)));
    mostPreciseBound((((0 + (D0p * C0)) + (D1p * C1)) < (((types * sep) + types) + 1)) || (((0 + (D0p * C0)) + (D1p * C1)) == (((types * sep) + types) + 1)));
    if (((sep < 0) || (sep == 0)) || ((types < 0) || (types == 0)))
    {
      return;
    }
    else
    {
      ;
    }
    int builder = 0;
    int R = 0;
    // mostPreciseBound(R <= types * sep + types + 1)
    // lessPreciseBound(R <= MAX * types * types + MAX * sep * sep + MAX * types * sep + MAX * types + MAX * sep + MAX)
    builder = builder + 1;
    D1 = D1 + 1;
    int i = 1;
    while (i < types)
    {
      builder = builder + sep;
      D0 = D0 + sep;
      builder = builder + 1;
      D1 = D1 + 1;
      i = i + 1;
      // reset R1
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