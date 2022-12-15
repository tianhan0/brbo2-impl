abstract class LenientFormat {
  void main(int template, int args) 

  {
    int C0 = -1;
    int D0 = 0;
    int D0p = 0;
    lessPreciseBound(((0 + D0) < (((8 * arraySum(template)) + (8 * args)) + 8)) || ((0 + D0) == (((8 * arraySum(template)) + (8 * args)) + 8)));
    mostPreciseBound(((0 + D0) < (((arraySum(template) + args) + 2) + (2 * args))) || ((0 + D0) == (((arraySum(template) + args) + 2) + (2 * args))));
    if (((arraySum(template) < 0) || (arraySum(template) == 0)) || ((args < 0) || (args == 0)))
    {
      return;
    }
    else
    {

    }
    int R = 0;
    // mostPreciseBound(R <= arraySum(template) + args + 2 + 2 * args)
    // lessPreciseBound(R <= MAX * arraySum(template) + MAX * args + MAX)
    int chunk = 0;
    int separator = 0;
    int i = 0;
    chunk = arrayRead(template, i);
    D0 = D0 + chunk;
    i = i + 1;
    while ((i + 1) < arrayLength(template))
    {
      // reset R0
      {
        separator = arrayRead(template, i);
        chunk = arrayRead(template, (i + 1));
        D0 = D0 + chunk;
        i = i + 2;
      }
    }
    if (i < args)
    {
      D0 = D0 + 1;
      D0 = D0 + 1;
      i = i + 1;
      while (i < args)
      {
        // reset R0
        {
          D0 = D0 + 1;
          D0 = D0 + 1;
          i = i + 1;
        }
      }
    }
    else
    {

    }
  }
  public abstract int ndInt();
  public abstract int ndInt2(int lower, int upper);
  public abstract boolean ndBool();
  public abstract void assume(boolean expression);
  public abstract void mostPreciseBound(boolean assertion);
  public abstract void lessPreciseBound(boolean assertion);
  public abstract void resetPlaceHolder();
}