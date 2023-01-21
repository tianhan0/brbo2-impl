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
        "--threads",
        type=int,
        required=False,
        default=4,
        help="The number of threads when decomposing a program.",
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
        "--deps",
        action="store_true",
        default=True,
        help="Whether to run the script that assumes needing the jar dependencies.",
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
        help="Whether to run QFuzz or the naive fuzzer.",
    )
    parser.set_defaults(dry=False)
    args = parser.parse_args()
    print_args(args)

    java_files = get_files(args.input, suffix="java")
    time_measurements = common.TimeMeasurement()
    brbo_root = Path(args.brbo).expanduser()
    for java_file in java_files:
        logging.info(f"Process file `{java_file}`")

        if args.mode == "qfuzz":
            _, fuzzing_time = run_command(
                command=common.qfuzz_command(
                    timeout=args.timeout, input=java_file, qfuzz=args.qfuzz, deps=args.deps
                ),
                dry=args.dry,
            )
        elif args.mode == "naive":
            fuzzing_time = 0
        else:
            raise AssertionError(f"Unknown mode {args.mode}")

        decomposed_file = get_decomposed_file(java_file=java_file)
        logging.info(f"Remove the expected decomposition `{str(decomposed_file)}`")
        run_command(command=["rm", str(decomposed_file)], dry=args.dry)

        _, decomposition_time = run_command(
            command=common.decomposition_command(
                threads=args.threads,
                input=java_file,
                samples=args.samples,
                deps=args.deps,
            ),
            dry=args.dry,
        )

        brbo_output, verification_time = run_command(
            command=common.verification_command(
                file=decomposed_file,
                icra=args.icra,
                deps=args.deps,
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
