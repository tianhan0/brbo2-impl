abstract class AppendQuotedString {
  void execute(int pattern, int stopIndex) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int R = 0;
    int temporaryStopIndex = 0;
    int lastIndexOfStopIndex = 0;
    lessPreciseBound((R < ((8 * pattern) + 8)) || (R == ((8 * pattern) + 8)));
    mostPreciseBound((R < pattern) || (R == pattern));
    if (((pattern < 0) || (pattern == 0)) || ((stopIndex < pattern) || (stopIndex == pattern)))
    {
      return;
    }
    else
    {
      ;
    }
    int appendTo = 0;
    // int R = 0;
    // mostPreciseBound(R <= pattern)
    // lessPreciseBound(R <= MAX * pattern + MAX)
    int pos = ndInt2(0, pattern - 1);
    int start = pos;
    int lastHold = start;
    int choice = 0;
    int i = pos;
    while (i < pattern)
    {
      {
        temporaryStopIndex = ndInt2(lastIndexOfStopIndex, stopIndex);
        choice = temporaryStopIndex - lastIndexOfStopIndex;
        lastIndexOfStopIndex = temporaryStopIndex;
      }
      if ((choice < 102) || (choice == 102))
      {
        pos = pos + 1;
        appendTo = appendTo + (pos - lastHold);
        R = R + (pos - lastHold);
        break;
      }
      else
      {
        ;
      }
      pos = pos + 1;
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