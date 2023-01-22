import argparse
import logging
from pathlib import Path
import common
from common import run_command, print_args, get_files, get_decomposed_file


if __name__ == "__main__":
    """
    Usage (Qfuzz): ~/brbo2-impl$ python3 scripts/brbo2.py \
      --input src/main/java/brbo/benchmarks/sas22/stac/TemplateEngine2.java \
      --qfuzz $HOME/Documents/workspace/qfuzz/ \
      --brbo $HOME/Documents/workspace/brbo-impl/ \
      --log brbo2.json \
      --mode qfuzz
    Usage (Naive fuzzer): ~/brbo2-impl$ python3 scripts/brbo2.py \
      --input src/main/java/brbo/benchmarks/sas22/stac/TemplateEngine2.java \
      --qfuzz $HOME/Documents/workspace/qfuzz/ \
      --brbo $HOME/Documents/workspace/brbo-impl/ \
      --log brbo2.json \
      --mode naive \
      --samples 5
    """
    parser = argparse.ArgumentParser(
        description="Run the brbo2 pipeline: Fuzz -> Decompose -> Verify."
    )
    parser.add_argument(
        "--timeout",
        type=int,
        required=False,
        default=30,
        help="The timeout for running AFL in QFuzz (in seconds).",
    )
    parser.add_argument(
        "--input", type=str, required=True, help="The file or the directory to analyze."
    )
    parser.add_argument(
        "--samples",
        type=int,
        default=0,  # Disable generating traces from the dumb fuzzer, to avoid complicating the experiment setup
        help="The number of samples when randomly generating inputs for decomposing a program.",
    )
    parser.add_argument(
        "--brbo", type=str, default="~/brbo-impl", help="The directory of brbo-impl."
    )
    parser.add_argument(
        "--qfuzz",
        type=str,
        default=Path("~/Documents/workspace/qfuzz_docker/").expanduser(),
        help="The directory of qfuzz.",
    )
    parser.add_argument(
        "--icra",
        type=str,
        default=Path("~/Documents/workspace/icra/icra").expanduser(),
        help="The directory of executable file icra.",
    )
    parser.add_argument(
        "--dry", action="store_true", help="Print the commands without executing them."
    )
    parser.add_argument(
        "--log",
        type=str,
        required=True,
        help="The file to write the measurements to.",
    )
    parser.add_argument(
        "--mode",
        choices=["qfuzz", "naive"],
        help="Whether to run the modified QFuzz or the naive QFuzz.",
    )
    parser.add_argument(
        "--version",
        type=str,
        default="master",
        help="Build and run the brbo2 jar file (with `sbt package`) with the specified git commit hash.",
    )
    parser.set_defaults(dry=False)
    args = parser.parse_args()
    print_args(args)

    if args.version == "master":
        commit_hash, _ = run_command(
            command=["git", "log", '--format="%H"', "-n", "1"], printOutput=False
        )
    else:
        commit_hash = args.version
    run_command(command=["git", "checkout", commit_hash], printOutput=False)
    logging.info(f"Build a new version of brbo2: {commit_hash}")
    run_command(command=["sbt", "package"])

    java_files = get_files(args.input, suffix="java")
    time_measurements = common.TimeMeasurement()
    brbo_root = Path(args.brbo).expanduser()
    for java_file in java_files:
        logging.info(f"Process file `{java_file}`")

        _, fuzzing_time = run_command(
            command=common.qfuzz_command(
                timeout=args.timeout,
                input=java_file,
                qfuzz=args.qfuzz,
                deps=True,
                mode=args.mode,
            ),
            dry=args.dry,
        )

        decomposed_file = get_decomposed_file(java_file=java_file)
        logging.info(f"Remove the expected decomposition `{str(decomposed_file)}`")
        run_command(command=["rm", str(decomposed_file)], dry=args.dry)

        _, decomposition_time = run_command(
            command=common.decomposition_command(
                threads=4,
                input=java_file,
                samples=args.samples,
                deps=True,
            ),
            dry=args.dry,
        )

        brbo_output, verification_time = run_command(
            command=common.verification_command(
                file=decomposed_file,
                icra=args.icra,
                deps=True,
                timeout=60,
                mode="transparent",
            ),
            cwd=brbo_root,
            dry=args.dry,
        )

        time_measurements.update(
            brbo_output=brbo_output,
            java_file=java_file,
            fuzzing_time=fuzzing_time,
            decomposition_time=decomposition_time,
            verification_time=verification_time,
        )

    time_measurements.print()
    time_measurements.write(log_file=args.log)
