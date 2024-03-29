# How To Load Apron Native Libraries.

1. Compile Apron.
    1. Install JDK.
    2. On Linux, run `sudo apt-get install libgmp-dev libmpfr-dev`. On Mac, run `brew install gmp` and `brew install mpfr`.
    3. On Linux, run `./configure -no-cxx -no-ocaml -no-ppl`. On Mac, run `./configure -no-cxx -no-ocaml -no-ppl --gmp-prefix /opt/homebrew/ --mpfr-prefix /opt/homebrew/`)
    4. `make`
    5. On Mac, check flag `jniinc` in `Makefile.config` to ensure it points to a directory that includes file `jni.h` and `jni_md.h` (which is in directory that looks like `~/.sdkman/candidates/java/current/include/darwin` and needs to be copied into `~/.sdkman/candidates/java/current/include/`). 
    6. `cd japron; make`
2. Copy all `*.so` files into `project_root/lib/`.
3. Compile all `*.java` files in `apron.jar` and `gmp.jar` into a jar file (e.g., `apronjava.jar`). It is so unexpected that after compiling Apron, the generated jar files only contain Java source code instead of `*.class` files! The generated jar file is needed both during the development and at runtime.
4. Set environment variable `LD_LIBRARY_PATH` in `build.sbt` to include the `*.so` files. By contrast, simply setting `-Djava.library.path` in `build.sbt` is not sufficient, because when loading `libjapron.so`, it will attempt to load `libgmp.so` (due to the dependency, which can be found by executing `ldd libjapron.so`). However, this fails because flag `-Djava.library.path` is meant for JVM but loading `libgmp.so` (when loading `libjapron.so`) is at the operating system level.

## References

1. https://stackoverflow.com/questions/28131096/cannot-open-shared-object-file-c-library-in-java
2. https://www.baeldung.com/linux/solve-shared-object-error
3. https://stackoverflow.com/questions/54373254/load-dependent-so-from-other-shared-library-via-jni