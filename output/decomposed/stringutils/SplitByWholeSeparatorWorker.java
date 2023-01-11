abstract class SplitByWholeSeparatorWorker {
  void execute(int str, int isSeparator, int max) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int C0 = -1;
    int D0 = 0;
    int D0p = 0;
    int temporaryStr = 0;
    int lastIndexOfStr = 0;
    lessPreciseBound(((0 + (D0p * C0)) < ((8 * str) + 8)) || ((0 + (D0p * C0)) == ((8 * str) + 8)));
    mostPreciseBound(((0 + (D0p * C0)) < str) || ((0 + (D0p * C0)) == str));
    if ((((str < 1) || (str == 1)) || ((max < 0) || (max == 0))) || !((!((isSeparator < 0)) && (isSeparator < str))))
    {
      return;
    }
    else
    {
      ;
    }
    int R = 0;
    // mostPreciseBound(R <= arraySum(str))
    // lessPreciseBound(R <= MAX * arraySum(str) + MAX)
    int numberOfStrings = 0;
    int chunk = 0;
    int isSeparatorChunk = 0;
    int i = 0;
    while (i < str)
    {
      {
        temporaryStr = ndInt2(lastIndexOfStr, str);
        chunk = temporaryStr - lastIndexOfStr;
        lastIndexOfStr = temporaryStr;
      }
      if (!((numberOfStrings < max)))
      {
        D0 = D0 + chunk;
        i = i + 1;
        continue;
      }
      else
      {
        ;
      }
      if (isSeparatorChunk < i)
      {
        numberOfStrings = numberOfStrings + 1;
      }
      else
      {
        D0 = D0 + chunk;
      }
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