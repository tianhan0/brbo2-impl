abstract class GetFormattedExceptionMessage {
  void execute(int baseMessage) 
  {
    int BOOLEAN_SEPARATOR = 502;
    int C0 = -1;
    int C1 = -1;
    int D0 = 0;
    int D0p = 0;
    int D1 = 0;
    int D1p = 0;
    int temporaryBaseMessage = 0;
    int lastIndexOfBaseMessage = 0;
    lessPreciseBound((((0 + (D0p * C0)) + (D1p * C1)) < (8 + (8 * baseMessage))) || (((0 + (D0p * C0)) + (D1p * C1)) == (8 + (8 * baseMessage))));
    mostPreciseBound((((0 + (D0p * C0)) + (D1p * C1)) < (3 + (6 * baseMessage))) || (((0 + (D0p * C0)) + (D1p * C1)) == (3 + (6 * baseMessage))));
    if ((baseMessage < 1) || (baseMessage == 1))
    {
      return;
    }
    else
    {
      ;
    }
    int buffer = 0;
    int R = 0;
    // mostPreciseBound(R <= 3 + 6 * arraySum(baseMessage))
    // lessPreciseBound(R <= MAX + MAX * arraySum(baseMessage))
    D0 = D0 + 1;
    D0 = D0 + 1;
    int chunk = 0;
    int i = 0;
    while (i < baseMessage)
    {
      {
        temporaryBaseMessage = ndInt2(lastIndexOfBaseMessage, baseMessage);
        chunk = temporaryBaseMessage - lastIndexOfBaseMessage;
        lastIndexOfBaseMessage = temporaryBaseMessage;
      }
      D0 = D0 + 1;
      D0 = D0 + 1;
      D0 = D0 + 1;
      D1 = D1 + chunk;
      D0 = D0 + 1;
      D0 = D0 + 1;
      D0 = D0 + 1;
      i = i + 1;
      // reset R1
      // reset R0
    }
    D0 = D0 + 1;
  }
  // Declare these methods such that these generated code can be parsed
  public abstract int ndInt();
  public abstract int ndInt2(int lower, int upper);
  public abstract boolean ndBool();
  public abstract void assume(boolean expression);
  public abstract void mostPreciseBound(boolean assertion);
  public abstract void lessPreciseBound(boolean assertion);
}