package brbo.benchmarks;

public abstract class Common {
  public int MAX = 8;
  public int LARGE_INT = 10000000;

  /**
   *
   * @return Non-deterministic integer
   */
  public abstract int ndInt();

  /**
   *
   * @param lower
   * @param upper
   * @return Return a non-deterministic integer in [lower, upper]
   */
  // public abstract int ndInt2(int lower, int upper);

  /**
   *
   * @param lower
   * @param x
   * @param upper
   * @return Assume x is in the range of [lower, upper]
   */
  public abstract void ndInt3(int lower, int x, int upper);

  /**
   *
   * @return Non-deterministic boolean
   */
  // public abstract boolean ndBool();

  public abstract void assume(boolean expression);

  /**
   *
   * @param args All heap locations (represented by variable names) that
   *             can reach the heap location pointed to by x, via accessing
   *             elements inside containers
   */
  public abstract void reach(int x, int... args);

  public abstract void boundAssertion(String index, boolean assertion);
}
