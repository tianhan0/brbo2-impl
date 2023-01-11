abstract class Join {
  void execute(int array, int startIndex, int endIndex) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int temporaryArray = 0;
    int lastIndexOfArray = 0;
    lessPreciseBound((R < ((8 + (8 * array)) + (8 * array))) || (R == ((8 + (8 * array)) + (8 * array))));
    mostPreciseBound((R < (array + array)) || (R == (array + array)));
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
    R = R + chunk;
    i = i + 1;
    while ((i < endIndex) && (i < array))
    {
      R = R + 1;
      {
        temporaryArray = ndInt2(lastIndexOfArray, array);
        chunk = temporaryArray - lastIndexOfArray;
        lastIndexOfArray = temporaryArray;
      }
      R = R + chunk;
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