import argparse
import logging
import os
from pathlib import Path
from common import run_command, print_args, configure_logging
from datetime import datetime


def brbo2_command(input, qfuzz, brbo, icra, dry, timeout, log_file, mode):
    brbo2_command = f"""python3 scripts/brbo2.py \
      --input {input} \
      --qfuzz {qfuzz} \
      --brbo {brbo} \
      --icra {icra} \
      --timeout {timeout} \
      --log {log_file} \
      --mode {mode} \
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

    icra_timeout_in_seconds = 60
    log_root_directory = Path(os.getcwd()) / "logs"
    qfuzz_log_directory = log_root_directory / "qfuzz"
    timeout_log_directory = log_root_directory / "timeout"
    verifiability_log_directory = log_root_directory / "verifiability"
    current_date_time = datetime.now().strftime("%Y%m%d_%H-%M-%S")

    if args.experiment == "verifiability":
        log_directory = verifiability_log_directory
    elif args.experiment == "qfuzz":
        log_directory = qfuzz_log_directory
    elif args.experiment == "timeout":
        log_directory = verifiability_log_directory
    elif args.experiment == "all":
        log_directory = log_root_directory
    else:
        raise AssertionError(f"Unknown experiment: {args.experiment}")
    configure_logging(
        filename=log_directory / f"experiment_{args.experiment}_{current_date_time}.txt"
    )
    print_args(args)

    for i in range(args.repeat):
        run_id = "{:03d}".format(i)
        logging.info(f"Begin {run_id} run")
        if args.experiment == "verifiability" or args.experiment == "all":
            current_log_directory = verifiability_log_directory / current_date_time

            selective_amortization = brbo2_command(
                input=args.input,
                qfuzz=args.qfuzz,
                brbo=args.brbo,
                icra=args.icra,
                dry=args.dry,
                timeout=60,
                log_file="a.json",
                mode="qfuzz",
            )
            run_command(command=selective_amortization, dry=args.dry)

            worst_case = brbo_command(
                input=args.input,
                brbo=args.brbo,
                icra=args.icra,
                dry=args.dry,
                timeout=icra_timeout_in_seconds,
                mode="worst",
                log_file="a.txt",
            )
            fully_amortized = brbo_command(
                input=args.input,
                brbo=args.brbo,
                icra=args.icra,
                dry=args.dry,
                timeout=icra_timeout_in_seconds,
                mode="fully",
                log_file="a.txt",
            )
        elif args.experiment == "qfuzz" or args.experiment == "all":
            timeout = 180
            current_log_directory = qfuzz_log_directory / current_date_time
            current_log_directory.mkdir(parents=True, exist_ok=True)

            naive_qfuzz = brbo2_command(
                input=args.input,
                qfuzz=args.qfuzz,
                brbo=args.brbo,
                icra=args.icra,
                dry=args.dry,
                timeout=timeout,
                log_file=current_log_directory / f"naive_{run_id}.txt",
                mode="naive",
            )
            run_command(command=naive_qfuzz, dry=args.dry)

            modified_qfuzz = brbo2_command(
                input=args.input,
                qfuzz=args.qfuzz,
                brbo=args.brbo,
                icra=args.icra,
                dry=args.dry,
                timeout=timeout,
                log_file=current_log_directory / f"qfuzz_{run_id}.txt",
                mode="qfuzz",
            )
            run_command(command=modified_qfuzz, dry=args.dry)
        elif args.experiment == "timeout" or args.experiment == "all":
            current_log_directory = timeout_log_directory / current_date_time
            current_log_directory.mkdir(parents=True, exist_ok=True)

            timeouts = [60, 120, 180]
            for timeout in timeouts:
                command = brbo2_command(
                    input=args.input,
                    qfuzz=args.qfuzz,
                    brbo=args.brbo,
                    icra=args.icra,
                    dry=args.dry,
                    timeout=timeout,
                    log_file=current_log_directory / f"timeout{timeout}_{run_id}.txt",
                    mode="qfuzz",
                )
                run_command(command=command, dry=args.dry)
