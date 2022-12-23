package brbo.fuzz;

public class Resource {
  public static void run(int[] array, int index) {
    int i = 0;
    while (i < array.length) {
      use(array[i]);
      index--;
      if (index == 0) {
        return;
      }
      i++;
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

  public static void run3(int[] array, int n, int index) {
    for (int j = 0; j < n; j++) {
      for (int i = 0; i < array.length; i++) {
        use(array[i]);
        index--;
        if (index == 0) {
          return;
        }
      }
    }
  }
  // 00112245: array is [00, 11, 22] and n is 45
  // 001122334456: array is [00, 11, 22, 33] and n is 56

  static void use(int n) {
    int i = 0;
    while (i < n) {
      i++;
    }
  }
}