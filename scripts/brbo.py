import argparse
import logging
import common
import sys
from common import (
    run_command,
    print_args,
    get_files,
    get_decomposed_file,
    configure_logging,
    sbt_package,
    interpret_brbo_output,
)
from pathlib import Path

if __name__ == "__main__":
    """
    Usage: ~/brbo2-impl$ python3 scripts/brbo.py \
      --input src/main/java/brbo/benchmarks/sas22/stac/TemplateEngine2.java \
      --brbo $HOME/Documents/workspace/brbo-impl/ \
      --timeout 60 \
      --mode worst \
      --log brbo.json
    """
    parser = argparse.ArgumentParser(
        description="Run the brbo pipeline: Translate with brbo2 -> Decompose -> Verify"
    )
    parser.add_argument(
        "--timeout",
        type=int,
        required=False,
        default=60,
        help="The timeout for running ICRA (in seconds).",
    )
    parser.add_argument(
        "--input", type=str, required=True, help="The file or the directory to analyze."
    )
    parser.add_argument(
        "--brbo", type=str, default="~/brbo-impl", help="The directory of brbo-impl."
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
        "--mode",
        choices=["worst", "fully"],
        required=True,
        help="The amortization mode.",
    )
    parser.add_argument(
        "--log",
        type=str,
        required=True,
        help="The file to write the measurements to.",
    )
    parser.add_argument(
        "--version",
        type=str,
        default="issta23",
        help="Build and run the brbo jar file (with `sbt package`) with the specified git commit hash.",
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
        raise AssertionError(f"Must specify a *.txt file name for --log.")
    configure_logging(filename=args.log)
    print_args(args)

    sbt_package_output = sbt_package(
        git_version=args.version, dry=args.dry, cwd=Path(args.brbo)
    )
    if "[success]" not in sbt_package_output:
        logging.error(f"Failed to run `sbt package`")
        sys.exit(-1)

    java_files = get_files(args.input, suffix="java")
    brbo_root = Path(args.brbo).expanduser()
    measurements = common.Measurement()
    for java_file in java_files:
        logging.info(f"Process file `{java_file}`")

        decomposed_file = get_decomposed_file(java_file=java_file)
        logging.info(f"Remove the existing decomposition `{str(decomposed_file)}`")
        run_command(command=["rm", str(decomposed_file)], dry=args.dry)

        # Translate the input java program into a brbo-compatible form
        run_command(
            command=common.translate_command(file=java_file, deps=True), dry=args.dry
        )

        if args.mode == "worst":
            mode = "no"
        elif args.mode == "fully":
            mode = "full"
        else:
            raise AssertionError(f"Unexpected mode {args.mode}")
        brbo_output, verification_time = run_command(
            command=common.verification_command(
                file=decomposed_file,
                icra=args.icra,
                deps=True,
                timeout=args.timeout,
                mode=mode,
                loose_bound=args.loose_bound,
            ),
            cwd=brbo_root,
            dry=args.dry,
        )
        measurements.update(
            verification_result=interpret_brbo_output(brbo_output),
            java_file=java_file,
            verification_time=verification_time,
        )

    measurements.print()
    measurements.write(log_file=Path(args.log).with_suffix(".json"))
