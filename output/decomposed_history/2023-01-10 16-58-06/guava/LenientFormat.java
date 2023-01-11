abstract class LenientFormat {
  void execute(int template, int args) 
  {
    int BOOLEAN_SEPARATOR = 102;
    int C0 = -1;
    int D0 = 0;
    int D0p = 0;
    int temporaryTemplate = 0;
    int lastIndexOfTemplate = 0;
    lessPreciseBound(((0 + (D0p * C0)) < (((8 * template) + (8 * args)) + 8)) || ((0 + (D0p * C0)) == (((8 * template) + (8 * args)) + 8)));
    mostPreciseBound(((0 + (D0p * C0)) < (((template + args) + 2) + (2 * args))) || ((0 + (D0p * C0)) == (((template + args) + 2) + (2 * args))));
    if (((template < 1) || (template == 1)) || ((args < 0) || (args == 0)))
    {
      return;
    }
    else
    {
      ;
    }
    int R = 0;
    // mostPreciseBound(R <= arraySum(template) + args + 2 + 2 * args)
    // lessPreciseBound(R <= MAX * arraySum(template) + MAX * args + MAX)
    int chunk = 0;
    int separator = 0;
    int i = 0;
    {
      temporaryTemplate = ndInt2(lastIndexOfTemplate, template);
      chunk = temporaryTemplate - lastIndexOfTemplate;
      lastIndexOfTemplate = temporaryTemplate;
    }
    D0 = D0 + chunk;
    i = i + 1;
    while ((i + 1) < template)
    {
      {
        temporaryTemplate = ndInt2(lastIndexOfTemplate, template);
        separator = temporaryTemplate - lastIndexOfTemplate;
        lastIndexOfTemplate = temporaryTemplate;
      }
      {
        temporaryTemplate = ndInt2(lastIndexOfTemplate, template);
        chunk = temporaryTemplate - lastIndexOfTemplate;
        lastIndexOfTemplate = temporaryTemplate;
      }
      D0 = D0 + chunk;
      i = i + 2;
      // reset R0
    }
    if (i < args)
    {
      R = R + 1;
      R = R + 1;
      i = i + 1;
      while (i < args)
      {
        R = R + 1;
        R = R + 1;
        i = i + 1;
        // resetPlaceHolder_2();
      }
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