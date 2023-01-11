abstract class Literal {
  void execute(int pattern, int currentIdx) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int C0 = -1;
    int D0 = 0;
    int D0p = 0;
    lessPreciseBound(((0 + (D0p * C0)) < ((8 * pattern) + 8)) || ((0 + (D0p * C0)) == ((8 * pattern) + 8)));
    mostPreciseBound(((0 + (D0p * C0)) < pattern) || ((0 + (D0p * C0)) == pattern));
    if ((((pattern < 0) || (pattern == 0)) || ((currentIdx < 0) || (currentIdx == 0))) || !((currentIdx < (pattern - 1))))
    {
      return;
    }
    else
    {
      ;
    }
    int sb = 0;
    int R = 0;
    // mostPreciseBound(R <= pattern)
    // lessPreciseBound(R <= MAX * pattern + MAX)
    while (currentIdx < pattern)
    {
      currentIdx = currentIdx + 1;
      sb = sb + 1;
      D0 = D0 + 1;
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