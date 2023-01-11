abstract class DiffResultToString {
  void execute(int diff) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int C0 = -1;
    int D0 = 0;
    int D0p = 0;
    int temporaryDiff = 0;
    int lastIndexOfDiff = 0;
    lessPreciseBound(((0 + (D0p * C0)) < ((8 * diff) + 8)) || ((0 + (D0p * C0)) == ((8 * diff) + 8)));
    mostPreciseBound(((0 + (D0p * C0)) < diff) || ((0 + (D0p * C0)) == diff));
    if ((diff < 1) || (diff == 1))
    {
      return;
    }
    else
    {
      ;
    }
    int lhsBuilder = 0;
    int rhsBuilder = 0;
    int R = 0;
    // mostPreciseBound(R <= arraySum(diff))
    // lessPreciseBound(R <= MAX * arraySum(diff) + MAX)
    int chunk = 0;
    int i = 0;
    while (i < diff)
    {
      {
        temporaryDiff = ndInt2(lastIndexOfDiff, diff);
        chunk = temporaryDiff - lastIndexOfDiff;
        lastIndexOfDiff = temporaryDiff;
      }
      D0 = D0 + chunk;
      i = i + 1;
      // reset R0
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