abstract class AppendDisplayNames {
  void execute(int sorted) 
  {
    int BOOLEAN_SEPARATOR = 502;
    int C0 = -1;
    int C1 = -1;
    int D0 = 0;
    int D0p = 0;
    int D1 = 0;
    int D1p = 0;
    int temporarySorted = 0;
    int lastIndexOfSorted = 0;
    lessPreciseBound((((0 + (D0p * C0)) + (D1p * C1)) < ((8 * sorted) + 8)) || (((0 + (D0p * C0)) + (D1p * C1)) == ((8 * sorted) + 8)));
    mostPreciseBound((((0 + (D0p * C0)) + (D1p * C1)) < (sorted + (2 * sorted))) || (((0 + (D0p * C0)) + (D1p * C1)) == (sorted + (2 * sorted))));
    if ((sorted < 1) || (sorted == 1))
    {
      return;
    }
    else
    {
      ;
    }
    int R = 0;
    // mostPreciseBound(R <= arraySum(sorted) + 2 * arraySum(sorted))
    // lessPreciseBound(R <= MAX * arraySum(sorted) + MAX)
    int entry = 0;
    int i = 0;
    while (i < sorted)
    {
      {
        temporarySorted = ndInt2(lastIndexOfSorted, sorted);
        entry = temporarySorted - lastIndexOfSorted;
        lastIndexOfSorted = temporarySorted;
      }
      int j = 0;
      while (j < entry)
      {
        D0 = D0 + 2;
        j = j + 1;
        // reset R1
        D0p = D0;
        D0 = 0;
        C0 = C0 + 1;
      }
      D1 = D1 + 1;
      i = i + 1;
      D1p = D1;
      D1 = 0;
      C1 = C1 + 1;
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