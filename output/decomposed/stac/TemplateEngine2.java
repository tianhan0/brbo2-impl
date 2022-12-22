abstract class TemplateEngine2 {
  void main(int text, int templateds, int separator) 

  {
    int C0 = -1;
    int C1 = -1;
    int D0 = 0;
    int D0p = 0;
    int D1 = 0;
    int D1p = 0;
    lessPreciseBound((((0 + D0) + D1) < (((((((((((8 * arraySum(text)) * templateds) + ((8 * separator) * templateds)) + ((8 * separator) * arraySum(text))) + ((8 * arraySum(text)) * arraySum(text))) + ((8 * separator) * separator)) + ((8 * templateds) * templateds)) + (8 * arraySum(text))) + (8 * templateds)) + (8 * separator)) + 8)) || (((0 + D0) + D1) == (((((((((((8 * arraySum(text)) * templateds) + ((8 * separator) * templateds)) + ((8 * separator) * arraySum(text))) + ((8 * arraySum(text)) * arraySum(text))) + ((8 * separator) * separator)) + ((8 * templateds) * templateds)) + (8 * arraySum(text))) + (8 * templateds)) + (8 * separator)) + 8)));
    mostPreciseBound((((0 + D0) + D1) < ((arraySum(text) * templateds) + (separator * templateds))) || (((0 + D0) + D1) == ((arraySum(text) * templateds) + (separator * templateds))));
    if ((((arraySum(text) < 0) || (arraySum(text) == 0)) || ((templateds < 0) || (templateds == 0))) || ((separator < 0) || (separator == 0)))
    {
      return;
    }
    else
    {

    }
    int R = 0;
    // mostPreciseBound(R <= arraySum(text) * templateds + separator * templateds)
    // lessPreciseBound(R <= MAX * arraySum(text) * templateds + MAX * separator * templateds + MAX * separator * arraySum(text) + MAX * arraySum(text) * arraySum(text) + MAX * separator * separator + MAX * templateds * templateds + MAX * arraySum(text) + MAX * templateds + MAX * separator + MAX)
    int i = 0;
    int j = 0;
    int chunk = 0;
    int tag = 0;
    while (i < templateds)
    {
      {
        j = 0;
        chunk = arrayRead(text, j);
        D1 = D1 + chunk;
        j = j + 1;
        while ((j + 1) < arrayLength(text))
        {
          {
            tag = arrayRead(text, j);
            chunk = arrayRead(text, (j + 1));
            D1 = D1 + chunk;
            j = j + 2;
          }
          {
            // reset R1
            // reset R0
          }
        }
        i = i + 1;
        D0 = D0 + separator;
      }
      {
        if (D1p < D1)
        {
          D1p = D1;
        }
        else
        {
          ;
        }
        D1 = 0;
        C1 = C1 + 1;
        if (D0p < D0)
        {
          D0p = D0;
        }
        else
        {
          ;
        }
        D0 = 0;
        C0 = C0 + 1;
      }
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