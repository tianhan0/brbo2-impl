import argparse
import logging
import common
from common import (
    run_command,
    print_args,
    get_java_files,
    get_decomposed_file,
)
from pathlib import Path

if __name__ == "__main__":
    """
    Usage: ~/brbo2-impl$ python3 scripts/brbo.py \
      --input src/main/java/brbo/benchmarks/sas22/stac/TemplateEngine2.java \
      --brbo $HOME/Documents/workspace/brbo-impl/ \
      --timeout 60
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
    parser.set_defaults(dry=False)
    args = parser.parse_args()
    print_args(args)

    java_files = get_java_files(args.input)
    brbo_root = Path(args.brbo).expanduser()
    time_measurements = common.TimeMeasurement()
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
            ),
            cwd=brbo_root,
            dry=args.dry,
        )
        time_measurements.update(
            brbo_output=brbo_output,
            java_file=java_file,
            verification_time=verification_time,
        )

    time_measurements.print()
