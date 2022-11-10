package brbo.benchmarks;

public abstract class Common {
    public int MAX = 8;
    public int LARGE_INT = 10000000;

    public abstract int arrayRead(int[] x, int index);

    public abstract int arrayLength(int[] x);

    public abstract int arraySum(int[] x);

    public abstract void use(int group, int cost, boolean condition);

    public abstract void use(int group, int cost);

    public abstract void reset(int group, boolean condition);

    public abstract void reset(int x);

    /**
     * @return Non-deterministic integer
     */
    public abstract int ndInt();

    /**
     * @param lower
     * @param upper
     * @return Return a non-deterministic integer in [lower, upper]
     */
    public abstract int ndInt2(int lower, int upper);

    /**
     * @return Non-deterministic boolean
     */
    public abstract boolean ndBool();

    public abstract void assume(boolean expression);

    public abstract void mostPreciseBound(boolean assertion);

    public abstract void lessPreciseBound(boolean assertion);

    public abstract void resetPlaceHolder();
}
