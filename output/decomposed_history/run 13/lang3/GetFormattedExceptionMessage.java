abstract class GetFormattedExceptionMessage {
  void execute(int baseMessage) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int R = 0;
    int temporaryBaseMessage = 0;
    int lastIndexOfBaseMessage = 0;
    lessPreciseBound((R < (8 + (8 * baseMessage))) || (R == (8 + (8 * baseMessage))));
    mostPreciseBound((R < (3 + (6 * baseMessage))) || (R == (3 + (6 * baseMessage))));
    if ((baseMessage < 1) || (baseMessage == 1))
    {
      return;
    }
    else
    {
      ;
    }
    int buffer = 0;
    // int R = 0;
    // mostPreciseBound(R <= 3 + 6 * arraySum(baseMessage))
    // lessPreciseBound(R <= MAX + MAX * arraySum(baseMessage))
    R = R + 1;
    R = R + 1;
    int chunk = 0;
    int i = 0;
    while (i < baseMessage)
    {
      {
        temporaryBaseMessage = ndInt2(lastIndexOfBaseMessage, baseMessage);
        chunk = temporaryBaseMessage - lastIndexOfBaseMessage;
        lastIndexOfBaseMessage = temporaryBaseMessage;
      }
      R = R + 1;
      R = R + 1;
      R = R + 1;
      R = R + chunk;
      R = R + 1;
      R = R + 1;
      R = R + 1;
      i = i + 1;
      // resetPlaceHolder_1();
    }
    R = R + 1;
  }
  // Declare these methods such that these generated code can be parsed
  public abstract int ndInt();
  public abstract int ndInt2(int lower, int upper);
  public abstract boolean ndBool();
  public abstract void assume(boolean expression);
  public abstract void mostPreciseBound(boolean assertion);
  public abstract void lessPreciseBound(boolean assertion);
}