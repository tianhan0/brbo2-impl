# Run QFuzz to Generate Interesting Inputs

1. Start a docker container:
    ```shell
    docker run -v $HOME/Documents:/root/Documents -it --rm yannicnoller/qfuzz
    ```
2. Open new terminals that connect to the above container:
   ```shell
   # Substitute the container ID
   docker exec -it 08a939169f97 bash
   ```
3. Compile the driver and the program of interest (that consumes resources), and then instrument the `*.class` files:
    ```shell
    ./scripts/prepare.sh
    ```
4. Start kelinci server that invokes the fuzzing engine and gives feedback about which inputs are interesting:
    ```shell
    ./scripts/server.sh
    ```
5. Start the fuzzing engine:
    ```shell
    ./scripts/afl.sh
    ```

## References

1. https://github.com/yannicnoller/qfuzz/blob/master/README.md
2. https://github.com/isstac/kelinci/blob/master/docs/ccs17-kersten.pdf