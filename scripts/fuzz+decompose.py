import argparse
import subprocess
import glob
import getpass
import os
import time
import logging
import sys
import json
from pathlib import Path


NO_DEPENDENCY_SCRIPT = "./scripts/run.sh"
WITH_DEPENDENCY_SCRIPT = "./scripts/run_deps.sh"

logging.basicConfig(
    stream=sys.stdout,
    encoding="utf-8",
    level=logging.DEBUG,
    format="%(asctime)s %(levelname)-8s %(message)s",
)


# Return stdout and stderr, and the execution time
def run_command(command, cwd=os.getcwd(), dry=False, printOutput=True):
    logging.info(f"Under `{getpass.getuser()}@{cwd}`: Execute `{' '.join(command)}`")
    if dry:
        return "", 0
    start_time = time.time()
    result = subprocess.run(
        command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, cwd=cwd
    )
    execution_time = time.time() - start_time
    logging.info(f"Done. Execution time: {execution_time} seconds")
    if printOutput:
        logging.info(f"Output: {result.stdout}")
    return result.stdout.strip(), execution_time


def qfuzz_command(timeout, input, qfuzz, deps):
    return [
        WITH_DEPENDENCY_SCRIPT if deps else NO_DEPENDENCY_SCRIPT,
        "fuzz",
        "--timeout",
        str(timeout),
        "--directory",
        str(input),
        # "-o",
        # "src/main/java/brbo/fuzz/",
        # "--dry",
        "--qfuzz",
        str(qfuzz),
    ]


def decomposition_command(threads, samples, input, deps):
    return [
        WITH_DEPENDENCY_SCRIPT if deps else NO_DEPENDENCY_SCRIPT,
        "decompose",
        "--threads",
        str(threads),
        "--debug",
        "--algorithm",
        "optics",
        "--parameter",
        str(0.1),
        "--samples",
        str(samples),
        "--directory",
        str(input),
    ]


def verification_command(decomposed_file, icra, deps):
    return [
        WITH_DEPENDENCY_SCRIPT if deps else NO_DEPENDENCY_SCRIPT,
        "--directory",
        str(decomposed_file),
        "--amortize",
        "transparent",
        "--icra-path",
        str(icra),
        "--icra-timeout",
        "60",
    ]


def increment_count(dictionary, key):
    dictionary.update({key: dictionary.get(key, 0) + 1})


def pretty_print(dictionary):
    return json.dumps(dictionary, indent=2)


if __name__ == "__main__":
    """
    Usage: ~/brbo2-impl$ python3 scripts/fuzz+decompose.py \
      --input src/main/java/brbo/benchmarks/sas22/stac/TemplateEngine2.java \
      --qfuzz $HOME/Documents/workspace/qfuzz/ \
      --brbo2 $HOME/Documents/workspace/brbo2-impl/ \
      --brbo $HOME/Documents/workspace/brbo-impl/
    """
    parser = argparse.ArgumentParser(
        description="Parse bytes from a file into a sequence of shorts."
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
        "--brbo2", type=str, default="~/brbo2-impl", help="The directory of brbo2-impl."
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
    parser.set_defaults(dry=False)
    args = parser.parse_args()
    for key, value in vars(args).items():
        logging.info(f"{key}\t{value}")

    java_files = []
    input_path = Path(args.input)
    if input_path.is_file():
        java_files.append(input_path.absolute())
    elif input_path.is_dir():
        for java_file in glob.iglob(args.input + "**/*.java", recursive=True):
            java_files.append(Path(java_file).absolute())
    else:
        logging.error(f"{input_path} is neither a file nor a directory")
        sys.exit(-1)

    brbo2_root = Path(args.brbo2).expanduser()
    brbo_root = Path(args.brbo).expanduser()

    java_files = sorted(java_files, key=lambda path: str(path), reverse=False)
    time_measurements = {}
    total_time = 0
    count_verified = {}
    count_not_verified = {}
    count_unknown = {}
    verification_results = {}
    for java_file in java_files:
        logging.info(f"Process file `{java_file}`")

        _, fuzzing_time = run_command(
            command=qfuzz_command(
                timeout=args.timeout, input=java_file, qfuzz=args.qfuzz, deps=args.deps
            ),
            cwd=brbo2_root,
            dry=args.dry,
        )

        inner_most_package_name = java_file.parent.parts[-1]
        decomposed_file_path = (
            brbo2_root / "output" / "decomposed" / inner_most_package_name
        )
        decomposed_file = decomposed_file_path / java_file.name
        logging.info(f"Remove the expected decomposition `{str(decomposed_file)}`")
        run_command(command=["rm", str(decomposed_file)], cwd=brbo2_root, dry=args.dry)

        _, decomposition_time = run_command(
            command=decomposition_command(
                threads=args.threads,
                input=java_file,
                samples=args.samples,
                deps=args.deps,
            ),
            cwd=brbo2_root,
            dry=args.dry,
        )

        brbo_output, verification_time = run_command(
            command=verification_command(
                decomposed_file=decomposed_file, icra=args.icra, deps=args.deps
            ),
            cwd=brbo_root,
            dry=args.dry,
        )

        if "verified? Yes" in brbo_output:
            increment_count(count_verified, inner_most_package_name)
            verification_results.update({str(java_file): "verified"})
        elif "verified? No" in brbo_output:
            increment_count(count_not_verified, inner_most_package_name)
            verification_results.update({str(java_file): "not verified"})
        else:
            increment_count(count_unknown, inner_most_package_name)
            verification_results.update({str(java_file): "unknown"})

        time_measurements.update(
            {str(java_file): (fuzzing_time, decomposition_time, verification_time)}
        )

        total_time = total_time + fuzzing_time + decomposition_time + verification_time

    logging.info(f"Execution time measurements:\n{pretty_print(time_measurements)}")
    logging.info(f"Total time elapsed: {total_time} seconds")

    logging.info(f"Verification results:\n{pretty_print(verification_results)}")
    logging.info(f"Number of verified programs: {pretty_print(count_verified)}")
    logging.info(f"Number of not verified programs: {pretty_print(count_not_verified)}")
    logging.info(f"Number of unknown programs: {pretty_print(count_unknown)}")
