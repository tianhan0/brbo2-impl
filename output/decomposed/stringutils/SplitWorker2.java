abstract class SplitWorker2 {
  void execute(int str, int separatorChars, int max, boolean preserveAllTokens, int choices) 
  {
    int BOOLEAN_SEPARATOR = 500;
    int C0 = -1;
    int C1 = -1;
    int D0 = 0;
    int D0p = 0;
    int D1 = 0;
    int D1p = 0;
    int temporaryChoices = 0;
    int lastIndexOfChoices = 0;
    lessPreciseBound((((0 + (D0p * C0)) + (D1p * C1)) < ((8 * str) + 8)) || (((0 + (D0p * C0)) + (D1p * C1)) == ((8 * str) + 8)));
    mostPreciseBound((((0 + (D0p * C0)) + (D1p * C1)) < str) || (((0 + (D0p * C0)) + (D1p * C1)) == str));
    if (((((str < 0) || (str == 0)) || ((max < 0) || (max == 0))) || (separatorChars < 0)) || (choices < str))
    {
      return;
    }
    else
    {
      ;
    }
    int R = 0;
    // mostPreciseBound(R <= str)
    // lessPreciseBound(R <= MAX * str + MAX)
    int list = 0;
    int sizePlus1 = 1;
    int i = 0;
    int start = 0;
    boolean match = false;
    boolean lastMatch = false;
    int choice = 0;
    if (separatorChars == 0)
    {
      while (i < str)
      {
        {
          temporaryChoices = ndInt2(lastIndexOfChoices, choices);
          choice = temporaryChoices - lastIndexOfChoices;
          lastIndexOfChoices = temporaryChoices;
        }
        if (!((choice < 500)) && !((choice == 500)))
        {
          if (match || preserveAllTokens)
          {
            lastMatch = true;
            if (sizePlus1 == max)
            {
              i = str;
              lastMatch = false;
            }
            else
            {
              ;
            }
            sizePlus1 = sizePlus1 + 1;
            list = list + (i - start);
            R = R + (i - start);
            match = false;
          }
          else
          {
            ;
          }
          i = i + 1;
          start = i;
          continue;
        }
        else
        {
          ;
        }
        lastMatch = false;
        match = true;
        i = i + 1;
        // resetPlaceHolder_1();
      }
    }
    else
    {
      if (separatorChars == 1)
      {
        while (i < str)
        {
          {
            temporaryChoices = ndInt2(lastIndexOfChoices, choices);
            choice = temporaryChoices - lastIndexOfChoices;
            lastIndexOfChoices = temporaryChoices;
          }
          if (!((choice < 500)) && !((choice == 500)))
          {
            if (match || preserveAllTokens)
            {
              lastMatch = true;
              if (sizePlus1 == max)
              {
                i = str;
                lastMatch = false;
              }
              else
              {
                ;
              }
              sizePlus1 = sizePlus1 + 1;
              list = list + (i - start);
              R = R + (i - start);
              match = false;
            }
            else
            {
              ;
            }
            i = i + 1;
            start = i;
            continue;
          }
          else
          {
            ;
          }
          lastMatch = false;
          match = true;
          i = i + 1;
          // resetPlaceHolder_2();
        }
      }
      else
      {
        while (i < str)
        {
          {
            temporaryChoices = ndInt2(lastIndexOfChoices, choices);
            choice = temporaryChoices - lastIndexOfChoices;
            lastIndexOfChoices = temporaryChoices;
          }
          if (!((choice < 500)) && !((choice == 500)))
          {
            if (match || preserveAllTokens)
            {
              lastMatch = true;
              if (sizePlus1 == max)
              {
                i = str;
                lastMatch = false;
              }
              else
              {
                ;
              }
              sizePlus1 = sizePlus1 + 1;
              list = list + (i - start);
              if ((i < 0) || (i == 0))
              {
                D1 = D1 + (i - start);
              }
              else
              {
                D0 = D0 + (i - start);
              }
              match = false;
            }
            else
            {
              ;
            }
            i = i + 1;
            start = i;
            continue;
          }
          else
          {
            ;
          }
          lastMatch = false;
          match = true;
          i = i + 1;
          // reset R0
        }
      }
    }
    if (match || (preserveAllTokens && lastMatch))
    {
      list = list + (i - start);
      R = R + (i - start);
    }
    else
    {
      ;
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