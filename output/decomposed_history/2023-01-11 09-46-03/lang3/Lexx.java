abstract class Lexx {
  void execute(int format) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int C0 = -1;
    int D0 = 0;
    int D0p = 0;
    lessPreciseBound(((0 + (D0 + (D0p * (C0 - 1)))) < ((8 * format) + 8)) || ((0 + (D0 + (D0p * (C0 - 1)))) == ((8 * format) + 8)));
    mostPreciseBound(((0 + (D0 + (D0p * (C0 - 1)))) < format) || ((0 + (D0 + (D0p * (C0 - 1)))) == format));
    if ((format < 0) || (format == 0))
    {
      return;
    }
    else
    {
      ;
    }
    int buffer = 0;
    int R = 0;
    // mostPreciseBound(R <= format)
    // lessPreciseBound(R <= MAX * format + MAX)
    int inLiteral = 0;
    int i = 0;
    while (i < format)
    {
      if ((!((inLiteral < 0)) && !((inLiteral == 0))) && !((i < 2)))
      {
        buffer = buffer + 1;
        R = R + 1;
        i = i + 1;
        continue;
      }
      else
      {
        ;
      }
      if (!((inLiteral < 0)) && !((inLiteral == 0)))
      {
        inLiteral = 0;
      }
      else
      {
        D0 = D0 + (0 - buffer);
        buffer = 0;
        inLiteral = 1;
      }
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