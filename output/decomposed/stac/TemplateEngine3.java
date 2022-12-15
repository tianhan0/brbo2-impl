abstract class TemplateEngine3 {
  void main(int text, int ts, int sep, int rep) 

  {
    int C0 = -1;
    int C1 = -1;
    int C2 = -1;
    int C3 = -1;
    int D0 = 0;
    int D0p = 0;
    int D1 = 0;
    int D1p = 0;
    int D2 = 0;
    int D2p = 0;
    int D3 = 0;
    int D3p = 0;
    lessPreciseBound((((((0 + D0) + D1) + D2) + D3) < (((((((((((((((((8 * arraySum(text)) * ts) + ((8 * arraySum(text)) * sep)) + ((8 * arraySum(text)) * arraySum(text))) + ((8 * arraySum(text)) * rep)) + ((8 * ts) * sep)) + ((8 * ts) * arraySum(text))) + ((8 * ts) * rep)) + ((8 * sep) * arraySum(text))) + ((8 * sep) * rep)) + ((8 * arraySum(text)) * rep)) + (8 * arraySum(text))) + (8 * ts)) + (8 * sep)) + (8 * arraySum(text))) + (8 * rep)) + 8)) || (((((0 + D0) + D1) + D2) + D3) == (((((((((((((((((8 * arraySum(text)) * ts) + ((8 * arraySum(text)) * sep)) + ((8 * arraySum(text)) * arraySum(text))) + ((8 * arraySum(text)) * rep)) + ((8 * ts) * sep)) + ((8 * ts) * arraySum(text))) + ((8 * ts) * rep)) + ((8 * sep) * arraySum(text))) + ((8 * sep) * rep)) + ((8 * arraySum(text)) * rep)) + (8 * arraySum(text))) + (8 * ts)) + (8 * sep)) + (8 * arraySum(text))) + (8 * rep)) + 8)));
    mostPreciseBound((((((0 + D0) + D1) + D2) + D3) < (ts * ((arraySum(text) + (arraySum(text) * rep)) + sep))) || (((((0 + D0) + D1) + D2) + D3) == (ts * ((arraySum(text) + (arraySum(text) * rep)) + sep))));
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
        // reset R3
        if ((i < 0) || (i == 0))
        {
          // reset R2
        }
        else
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
        }
        if ((j < 2) || (j == 2))
        {
          // reset R1
        }
        else
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
        }
        // reset R0
      }
      {
        j = 0;
        chunk = arrayRead(text, j);
        D2 = D2 + chunk;
        j = j + 1;
        while ((j + 1) < arrayLength(text))
        {
          {
            // reset R2
            if ((i < 0) || (i == 0))
            {
              // reset R3
            }
            else
            {
              if ((j < 2) || (j == 2))
              {
                if (D3p < D3)
                {
                  D3p = D3;
                }
                else
                {
                  ;
                }
                D3 = 0;
                C3 = C3 + 1;
              }
              else
              {
                // reset R3
              }
            }
            // reset R1
            if ((j < 2) || (j == 2))
            {
              if ((i < 0) || (i == 0))
              {
                // reset R0
              }
              else
              {
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
            else
            {
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
          {
            tag = arrayRead(text, j);
            chunk = arrayRead(text, (j + 1));
            D3 = D3 + chunk;
            D0 = D0 + rep;
            j = j + 2;
          }
        }
        i = i + 1;
        D1 = D1 + sep;
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