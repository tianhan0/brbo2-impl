abstract class AppendRecursiveTypes {
  void execute(int argumentTypes, int recursiveTypeIndexes, int d) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int R = 0;
    lessPreciseBound((R < (((8 * recursiveTypeIndexes) + (8 * argumentTypes)) + 8)) || (R == (((8 * recursiveTypeIndexes) + (8 * argumentTypes)) + 8)));
    mostPreciseBound((R < ((recursiveTypeIndexes + 1) + (argumentTypes * 3))) || (R == ((recursiveTypeIndexes + 1) + (argumentTypes * 3))));
    if ((((argumentTypes < 0) || (argumentTypes == 0)) || ((recursiveTypeIndexes < 0) || (recursiveTypeIndexes == 0))) || !((((d < recursiveTypeIndexes) || (d == recursiveTypeIndexes)) && !((d < 0)))))
    {
      return;
    }
    else
    {
      ;
    }
    int builder = 0;
    // int R = 0;
    // mostPreciseBound(R <= recursiveTypeIndexes + 1 + argumentTypes * 3)
    // lessPreciseBound(R <= MAX * recursiveTypeIndexes + MAX * argumentTypes + MAX)
    int i = 0;
    while (i < recursiveTypeIndexes)
    {
      int sep = 2;
      builder = builder + 1;
      R = R + 1;
      int j = 1;
      while (j < 1)
      {
        builder = builder + sep;
        R = R + sep;
        builder = builder + 1;
        R = R + 1;
        j = j + 1;
        // resetPlaceHolder_2();
      }
      i = i + 1;
      // resetPlaceHolder_1();
    }
    int argumentsFiltered = argumentTypes - d;
    if (!((argumentsFiltered < 0)) && !((argumentsFiltered == 0)))
    {
      builder = builder + 1;
      R = R + 1;
      int sep2 = 2;
      int k = 1;
      while (k < argumentsFiltered)
      {
        builder = builder + sep2;
        R = R + sep2;
        builder = builder + 1;
        R = R + 1;
        k = k + 1;
        // resetPlaceHolder_3();
      }
    }
    else
    {
      ;
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