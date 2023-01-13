abstract class DurationFormat {
  void execute(int tokens, int years, int months) 
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
    int temporaryTokens = 0;
    int lastIndexOfTokens = 0;
    lessPreciseBound(((((0 + (D0 + (D0p * (C0 - 1)))) + (D1 + (D1p * (C1 - 1)))) + (D2 + (D2p * (C2 - 1)))) < ((((((((((8 * tokens) * tokens) + ((8 * years) * years)) + ((8 * months) * months)) + ((8 * tokens) * years)) + ((8 * tokens) * months)) + (8 * months)) + (8 * years)) + (8 * tokens)) + 8)) || ((((0 + (D0 + (D0p * (C0 - 1)))) + (D1 + (D1p * (C1 - 1)))) + (D2 + (D2p * (C2 - 1)))) == ((((((((((8 * tokens) * tokens) + ((8 * years) * years)) + ((8 * months) * months)) + ((8 * tokens) * years)) + ((8 * tokens) * months)) + (8 * months)) + (8 * years)) + (8 * tokens)) + 8)));
    mostPreciseBound(((((0 + (D0 + (D0p * (C0 - 1)))) + (D1 + (D1p * (C1 - 1)))) + (D2 + (D2p * (C2 - 1)))) < (tokens + (tokens * (years + months)))) || ((((0 + (D0 + (D0p * (C0 - 1)))) + (D1 + (D1p * (C1 - 1)))) + (D2 + (D2p * (C2 - 1)))) == (tokens + (tokens * (years + months)))));
    if ((((tokens < 1) || (tokens == 1)) || ((years < 0) || (years == 0))) || ((months < 0) || (months == 0)))
    {
      return;
    }
    else
    {
      ;
    }
    int R = 0;
    // mostPreciseBound(R <= arraySum(tokens) + arraySum(tokens) * (years + months))
    // lessPreciseBound(R <= MAX * arraySum(tokens) * arraySum(tokens) + MAX * years * years + MAX * months * months + MAX * arraySum(tokens) * years + MAX * arraySum(tokens) * months + MAX * months + MAX * years + MAX * arraySum(tokens) + MAX)
    int chunk = 0;
    int i = 0;
    while (i < tokens)
    {
      {
        temporaryTokens = ndInt2(lastIndexOfTokens, tokens);
        chunk = temporaryTokens - lastIndexOfTokens;
        lastIndexOfTokens = temporaryTokens;
      }
      D2 = D2 + chunk;
      D0 = D0 + years;
      D1 = D1 + months;
      i = i + 1;
      // reset R2
      D1p = D1;
      D1 = 0;
      C1 = C1 + 1;
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