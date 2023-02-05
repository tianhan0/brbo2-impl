import argparse
import logging
import sys
from pathlib import Path
import common
from common import (
    run_command,
    print_args,
    get_files,
    get_decomposed_file,
    configure_logging,
    sbt_package,
    interpret_brbo_output,
    get_trace_clusters,
    get_trace_inputs,
    get_decision_tree_predicate_count,
)


if __name__ == "__main__":
    """
    Usage (Qfuzz): ~/brbo2-impl$ python3 scripts/brbo2.py \
      --input src/main/java/brbo/benchmarks/sas22/stac/TemplateEngine2.java \
      --qfuzz $HOME/Documents/workspace/qfuzz/ \
      --brbo $HOME/Documents/workspace/brbo-impl/ \
      --log a.txt \
      --mode qfuzz
    Usage (Naive fuzzer): ~/brbo2-impl$ python3 scripts/brbo2.py \
      --input src/main/java/brbo/benchmarks/sas22/stac/TemplateEngine2.java \
      --qfuzz $HOME/Documents/workspace/qfuzz/ \
      --brbo $HOME/Documents/workspace/brbo-impl/ \
      --log a.txt \
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
    parser.add_argument(
        "--seed",
        type=str,
        default=None,
        help="The file that contains the seed for QFuzz.",
    )
    parser.add_argument(
        "--min-int",
        type=int,
        default=4,
        help="The max integer for QFuzz to find inputs from.",
    )
    parser.add_argument(
        "--max-int",
        type=int,
        default=12,
        help="The min integer for QFuzz to find inputs from.",
    )
    parser.add_argument(
        "--loose-bound",
        default=False,
        action="store_true",
        help="Verify the less precise bounds (as opposed to the most precise bounds).",
    )
    parser.set_defaults(dry=False)
    args = parser.parse_args()
    if Path(args.log).suffix != ".txt":
        raise AssertionError(
            f"Must specify a *.txt file name for --log: {Path(args.log).suffix}"
        )
    configure_logging(filename=args.log)
    print_args(args)

    sbt_package_output = sbt_package(git_version=args.version, dry=args.dry)
    if "[success]" not in sbt_package_output:
        logging.error(f"Failed to run `sbt package`")
        sys.exit(-1)

    java_files = get_files(args.input, suffix="java")
    measurements = common.Measurement()
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
                seed=args.seed,
                max_int=args.max_int,
                min_int=args.min_int,
            ),
            dry=args.dry,
            timeout=f"{int(args.timeout)+3}s",
        )

        decomposed_file = get_decomposed_file(java_file=java_file)
        logging.info(f"Remove the expected decomposition `{str(decomposed_file)}`")
        decomposed_file.unlink(missing_ok=True)

        decomposition_result, decomposition_time = run_command(
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
                loose_bound=args.loose_bound,
            ),
            cwd=brbo_root,
            dry=args.dry,
        )

        invariant_inference_failure = "Infer bound `true` for variable" in brbo_output
        if invariant_inference_failure:
            logging.info(f"Failed to infer invariants")
        measurements.update(
            verification_result=interpret_brbo_output(brbo_output),
            java_file=java_file,
            fuzzing_time=fuzzing_time,
            decomposition_time=decomposition_time,
            verification_time=verification_time,
            trace_clusters=get_trace_clusters(decomposition_result),
            trace_inputs=get_trace_inputs(decomposition_result),
            invariant_inference_failure=invariant_inference_failure,
            decision_tree_predicate_count=get_decision_tree_predicate_count(
                decomposition_result
            ),
        )
        measurements.print_concise()

    measurements.print()
    measurements.write(log_file=Path(args.log).with_suffix(".json"))
