abstract class TemplateEngine3 {
  void main(int text, int ts, int sep, int rep) 

  {
    int C0 = -1;
    int C1 = -1;
    int C2 = -1;
    int D0 = 0;
    int D0p = 0;
    int D1 = 0;
    int D1p = 0;
    int D2 = 0;
    int D2p = 0;
    lessPreciseBound(((((0 + D0) + D1) + D2) < (((((((((((((((((8 * arraySum(text)) * ts) + ((8 * arraySum(text)) * sep)) + ((8 * arraySum(text)) * arraySum(text))) + ((8 * arraySum(text)) * rep)) + ((8 * ts) * sep)) + ((8 * ts) * arraySum(text))) + ((8 * ts) * rep)) + ((8 * sep) * arraySum(text))) + ((8 * sep) * rep)) + ((8 * arraySum(text)) * rep)) + (8 * arraySum(text))) + (8 * ts)) + (8 * sep)) + (8 * arraySum(text))) + (8 * rep)) + 8)) || ((((0 + D0) + D1) + D2) == (((((((((((((((((8 * arraySum(text)) * ts) + ((8 * arraySum(text)) * sep)) + ((8 * arraySum(text)) * arraySum(text))) + ((8 * arraySum(text)) * rep)) + ((8 * ts) * sep)) + ((8 * ts) * arraySum(text))) + ((8 * ts) * rep)) + ((8 * sep) * arraySum(text))) + ((8 * sep) * rep)) + ((8 * arraySum(text)) * rep)) + (8 * arraySum(text))) + (8 * ts)) + (8 * sep)) + (8 * arraySum(text))) + (8 * rep)) + 8)));
    mostPreciseBound(((((0 + D0) + D1) + D2) < (ts * ((arraySum(text) + (arraySum(text) * rep)) + sep))) || ((((0 + D0) + D1) + D2) == (ts * ((arraySum(text) + (arraySum(text) * rep)) + sep))));
    if (((((arraySum(text) < 0) || (arraySum(text) == 0)) || ((ts < 0) || (ts == 0))) || ((sep < 0) || (sep == 0))) || ((rep < 0) || (rep == 0)))
    {
      return;
    }
    else
    {

    }
    int R = 0;
    // mostPreciseBound(R <= ts * (arraySum(text) + arraySum(text) * rep + sep))
    // lessPreciseBound(R <= MAX * arraySum(text) * ts + MAX * arraySum(text) * sep + MAX * arraySum(text) * arraySum(text) + MAX * arraySum(text) * rep + MAX * ts * sep + MAX * ts * arraySum(text) + MAX * ts * rep + MAX * sep * arraySum(text) + MAX * sep * rep + MAX * arraySum(text) * rep + MAX * arraySum(text) + MAX * ts + MAX * sep + MAX * arraySum(text) + MAX * rep + MAX)
    int i = 0;
    int j = 0;
    int chunk = 0;
    int tag = 0;
    while (i < ts)
    {
      {
        j = 0;
        chunk = arrayRead(text, j);
        D2 = D2 + chunk;
        j = j + 1;
        while ((j + 1) < arrayLength(text))
        {
          {
            tag = arrayRead(text, j);
            chunk = arrayRead(text, (j + 1));
            D2 = D2 + chunk;
            D0 = D0 + rep;
            j = j + 2;
          }
          {
            // reset R2
            // reset R1
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
        i = i + 1;
        D1 = D1 + sep;
      }
      {
        if (D2p < D2)
        {
          D2p = D2;
        }
        else
        {
          ;
        }
        D2 = 0;
        C2 = C2 + 1;
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
        // reset R0
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