abstract class SplitWorker2 {
  void execute(int str, int separatorChars, int max, boolean preserveAllTokens, int choice) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int R = 0;
    lessPreciseBound((R < ((8 * str) + 8)) || (R == ((8 * str) + 8)));
    mostPreciseBound((R < str) || (R == str));
    if (((((str < 0) || (str == 0)) || ((max < 0) || (max == 0))) || (separatorChars < 0)) || !((!((choice < 0)) && (choice < str))))
    {
      return;
    }
    else
    {
      ;
    }
    // int R = 0;
    // mostPreciseBound(R <= str)
    // lessPreciseBound(R <= MAX * str + MAX)
    int list = 0;
    int sizePlus1 = 1;
    int i = 0;
    int start = 0;
    boolean match = false;
    boolean lastMatch = false;
    if (separatorChars == 0)
    {
      while (i < str)
      {
        if (choice < i)
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
          if (choice < i)
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
          if (choice < i)
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
          // resetPlaceHolder_3();
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