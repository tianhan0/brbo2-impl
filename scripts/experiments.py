import argparse
import logging
from pathlib import Path
from common import run_command, print_args


def brbo2_command(input, qfuzz, brbo, icra, dry, timeout, log_file):
    brbo2_command = f"""python3 scripts/brbo2.py \
      --input {input} \
      --qfuzz {qfuzz} \
      --brbo {brbo} \
      --icra {icra} \
      --timeout {timeout} \
      --log {log_file} \
      {"--dry" if dry else ""}"""
    return brbo2_command.split()


def brbo_command(input, brbo, icra, dry, timeout, mode, log_file):
    brbo_command = f"""python3 scripts/brbo.py \
      --input {input} \
      --brbo {brbo} \
      --icra {icra} \
      --timeout {timeout} \
      --mode {mode} \
      --log {log_file} \
      {"--dry" if dry else ""}"""
    return brbo_command.split()


if __name__ == "__main__":
    """
    Usage: ~/brbo2-impl$ python3 scripts/experiments.py \
      --input src/main/java/brbo/benchmarks/sas22/stac/TemplateEngine2.java \
      --qfuzz $HOME/Documents/workspace/qfuzz/ \
      --brbo $HOME/Documents/workspace/brbo-impl/ \
      --experiment verifiability \
      --repeat 30
    """
    parser = argparse.ArgumentParser(
        description="Run the experiments and generate tables."
    )
    parser.add_argument(
        "--input",
        type=str,
        required=True,
        help="The file or the directory to run experiments against.",
    )
    parser.add_argument(
        "--experiment",
        choices=["verifiability", "qfuzz", "timeout", "all"],
        required=True,
        help="Which experiment to run.",
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
        "--repeat",
        type=int,
        default=3,
        help="The number of times to repeat the experiment.",
    )
    parser.set_defaults(dry=False)
    args = parser.parse_args()
    print_args(args)

    icra_timeout_in_seconds = 60
    for i in range(args.repeat):
        logging.info(f"{i} run")
        if args.experiment == "verifiability" or args.experiment == "all":
            selective_amortization = brbo2_command(
                input=args.input,
                qfuzz=args.qfuzz,
                brbo=args.brbo,
                icra=args.icra,
                dry=args.dry,
                timeout=60,
                log_file="a.json",
            )
            run_command(command=selective_amortization)

            worst_case = brbo_command(
                input=args.input,
                brbo=args.brbo,
                icra=args.icra,
                dry=args.dry,
                timeout=icra_timeout_in_seconds,
                mode="worst",
                log_file="a.json",
            )
            fully_amortized = brbo_command(
                input=args.input,
                brbo=args.brbo,
                icra=args.icra,
                dry=args.dry,
                timeout=icra_timeout_in_seconds,
                mode="fully",
                log_file="a.json",
            )
        elif args.experiment == "qfuzz" or args.experiment == "all":
            unmodified = []
            modified = []
        elif args.experiment == "timeout" or args.experiment == "all":
            timeout_30 = brbo2_command(
                input=args.input,
                qfuzz=args.qfuzz,
                brbo=args.brbo,
                icra=args.icra,
                dry=args.dry,
                timeout=30,
                log_file="a.json",
            )
