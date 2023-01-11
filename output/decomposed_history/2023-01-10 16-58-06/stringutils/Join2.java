abstract class Join2 {
  void execute(int objects) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int C0 = -1;
    int C1 = -1;
    int C2 = -1;
    int D0 = 0;
    int D0p = 0;
    int D1 = 0;
    int D1p = 0;
    int D2 = 0;
    int D2p = 0;
    int temporaryObjects = 0;
    int lastIndexOfObjects = 0;
    lessPreciseBound(((((0 + (D0p * C0)) + (D1p * C1)) + (D2p * C2)) < ((8 + (8 * objects)) + (8 * objects))) || ((((0 + (D0p * C0)) + (D1p * C1)) + (D2p * C2)) == ((8 + (8 * objects)) + (8 * objects))));
    mostPreciseBound(((((0 + (D0p * C0)) + (D1p * C1)) + (D2p * C2)) < (objects + objects)) || ((((0 + (D0p * C0)) + (D1p * C1)) + (D2p * C2)) == (objects + objects)));
    if ((objects < 1) || (objects == 1))
    {
      return;
    }
    else
    {
      ;
    }
    int R = 0;
    // mostPreciseBound(R <= arraySum(objects) + arraySum(objects))
    // lessPreciseBound(R <= MAX + MAX * arraySum(objects) + MAX * arraySum(objects))
    int i = 0;
    int chunk = 0;
    {
      temporaryObjects = ndInt2(lastIndexOfObjects, objects);
      chunk = temporaryObjects - lastIndexOfObjects;
      lastIndexOfObjects = temporaryObjects;
    }
    D2 = D2 + chunk;
    i = i + 1;
    while (i < objects)
    {
      D0 = D0 + 1;
      {
        temporaryObjects = ndInt2(lastIndexOfObjects, objects);
        chunk = temporaryObjects - lastIndexOfObjects;
        lastIndexOfObjects = temporaryObjects;
      }
      if ((i < 3) || (i == 3))
      {
        D1 = D1 + chunk;
      }
      else
      {
        D2 = D2 + chunk;
      }
      i = i + 1;
      // reset R2
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