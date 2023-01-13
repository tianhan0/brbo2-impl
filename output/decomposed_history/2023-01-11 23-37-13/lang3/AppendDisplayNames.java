abstract class AppendDisplayNames {
  void execute(int sorted) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int temporarySorted = 0;
    int lastIndexOfSorted = 0;
    lessPreciseBound((R < ((8 * sorted) + 8)) || (R == ((8 * sorted) + 8)));
    mostPreciseBound((R < (sorted + (2 * sorted))) || (R == (sorted + (2 * sorted))));
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
        R = R + 2;
        j = j + 1;
        // resetPlaceHolder_2();
      }
      R = R + 1;
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