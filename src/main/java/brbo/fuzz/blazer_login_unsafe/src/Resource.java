package brbo.fuzz.blazer_login_unsafe.src;

public class Resource {
  public static void run(int[] array, int index) {
    int i = 0;
    while (i < array.length) {
      i++;
      use(array[i]);
      index--;
      if (index == 0) {
        return;
      }
    }
  }

  public static void run2(int n, int m) {
    if (m > 1000) {
      int i = 0;
      while (i < n) {
        i++;
        use(1);
      }
    } else {
      use(n);
    }
  }

  static void use(int n) {
    int i = 0;
    while (i < n) {
      i++;
    }
  }
}