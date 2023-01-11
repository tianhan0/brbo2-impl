abstract class Join {
  void execute(int array, int startIndex, int endIndex) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int C0 = -1;
    int C1 = -1;
    int D0 = 0;
    int D0p = 0;
    int D1 = 0;
    int D1p = 0;
    int temporaryArray = 0;
    int lastIndexOfArray = 0;
    lessPreciseBound((((0 + (D0p * C0)) + (D1p * C1)) < ((8 + (8 * array)) + (8 * array))) || (((0 + (D0p * C0)) + (D1p * C1)) == ((8 + (8 * array)) + (8 * array))));
    mostPreciseBound((((0 + (D0p * C0)) + (D1p * C1)) < (array + array)) || (((0 + (D0p * C0)) + (D1p * C1)) == (array + array)));
    if (((((((array < 1) || (array == 1)) || ((startIndex < 0) || (startIndex == 0))) || !((startIndex < array))) || ((endIndex < 0) || (endIndex == 0))) || !((endIndex < array))) || (((endIndex - startIndex) < 0) || ((endIndex - startIndex) == 0)))
    {
      return;
    }
    else
    {
      ;
    }
    int R = 0;
    // mostPreciseBound(R <= arraySum(array) + arraySum(array))
    // lessPreciseBound(R <= MAX + MAX * arraySum(array) + MAX * arraySum(array))
    int i = startIndex;
    int chunk = 0;
    {
      temporaryArray = ndInt2(lastIndexOfArray, array);
      chunk = temporaryArray - lastIndexOfArray;
      lastIndexOfArray = temporaryArray;
    }
    D1 = D1 + chunk;
    i = i + 1;
    while ((i < endIndex) && (i < array))
    {
      D0 = D0 + 1;
      {
        temporaryArray = ndInt2(lastIndexOfArray, array);
        chunk = temporaryArray - lastIndexOfArray;
        lastIndexOfArray = temporaryArray;
      }
      D1 = D1 + chunk;
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