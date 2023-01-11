abstract class AppendQuotedString {
  void execute(int pattern, int pos, int stopIndex) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int C0 = -1;
    int D0 = 0;
    int D0p = 0;
    lessPreciseBound(((0 + (D0p * C0)) < (((8 * pattern) + (8 * pos)) + 8)) || ((0 + (D0p * C0)) == (((8 * pattern) + (8 * pos)) + 8)));
    mostPreciseBound(((0 + (D0p * C0)) < pattern) || ((0 + (D0p * C0)) == pattern));
    if (((((pattern < 0) || (pattern == 0)) || ((pos < 0) || (pos == 0))) || (pattern < pos)) || !((((stopIndex < pattern) || (stopIndex == pattern)) && !((stopIndex < pos)))))
    {
      return;
    }
    else
    {
      ;
    }
    int appendTo = 0;
    int R = 0;
    // mostPreciseBound(R <= pattern)
    // lessPreciseBound(R <= MAX * pattern + MAX * pos + MAX)
    int start = pos;
    int lastHold = start;
    int i = pos;
    while (i < pattern)
    {
      if (i == stopIndex)
      {
        pos = pos + 1;
        appendTo = appendTo + (pos - lastHold);
        D0 = D0 + (pos - lastHold);
        break;
      }
      else
      {
        ;
      }
      pos = pos + 1;
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