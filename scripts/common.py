import logging
import sys
import json
import getpass
import time
import subprocess
import os
import glob
from pathlib import Path

logging.basicConfig(
    stream=sys.stdout,
    encoding="utf-8",
    level=logging.DEBUG,
    format="%(asctime)s %(levelname)-8s %(message)s",
)

NO_DEPENDENCY_SCRIPT = "./scripts/run.sh"
WITH_DEPENDENCY_SCRIPT = "./scripts/run_deps.sh"


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


def verification_command(file, icra, deps, timeout, mode):
    return [
        WITH_DEPENDENCY_SCRIPT if deps else NO_DEPENDENCY_SCRIPT,
        "--directory",
        str(file),
        "--amortize",
        mode,
        "--icra-path",
        str(icra),
        "--icra-timeout",
        str(timeout),
    ]


# Translate the java file into a brbo compatible form
def translate_command(file, deps):
    return [
        WITH_DEPENDENCY_SCRIPT if deps else NO_DEPENDENCY_SCRIPT,
        "brbo",
        "--directory",
        str(file),
    ]


def pretty_print(dictionary):
    return json.dumps(dictionary, indent=2)


def get_inner_most_package_name(java_file_path: Path) -> str:
    return java_file_path.parent.parts[-1]


def _increment_count(dictionary, key):
    dictionary.update({key: dictionary.get(key, 0) + 1})


class TimeMeasurement:
    def __init__(self):
        self.per_file_execution_time = {}
        self.total_time = 0
        self.count_verified = {}
        self.count_not_verified = {}
        self.count_unknown = {}
        self.verification_results = {}

    def update(
        self,
        brbo_output,
        java_file,
        fuzzing_time=0,
        decomposition_time=0,
        verification_time=0,
    ):
        inner_most_package_name = get_inner_most_package_name(java_file)
        if "verified? Yes" in brbo_output:
            _increment_count(self.count_verified, inner_most_package_name)
            self.verification_results.update({str(java_file): "verified"})
        elif "verified? No" in brbo_output:
            _increment_count(self.count_not_verified, inner_most_package_name)
            self.verification_results.update({str(java_file): "not verified"})
        else:
            _increment_count(self.count_unknown, inner_most_package_name)
            self.verification_results.update({str(java_file): "unknown"})

        self.per_file_execution_time.update(
            {str(java_file): (fuzzing_time, decomposition_time, verification_time)}
        )
        self.total_time = (
            self.total_time + fuzzing_time + decomposition_time + verification_time
        )

    def print(self):
        logging.info(
            f"Execution time measurements:\n{pretty_print(self.per_file_execution_time)}"
        )
        logging.info(f"Total time elapsed: {self.total_time} seconds")

        logging.info(
            f"Verification results:\n{pretty_print(self.verification_results)}"
        )
        logging.info(
            f"Number of verified programs: {pretty_print(self.count_verified)}"
        )
        logging.info(
            f"Number of not verified programs: {pretty_print(self.count_not_verified)}"
        )
        logging.info(f"Number of unknown programs: {pretty_print(self.count_unknown)}")


def print_args(args):
    logging.info("Arguments:")
    for key, value in vars(args).items():
        logging.info(f"{key}\t{value}")


def get_java_files(path: str):
    java_files = []
    input_path = Path(path)
    if input_path.is_file():
        java_files.append(input_path.absolute())
    elif input_path.is_dir():
        for java_file in input_path.rglob("*.java"):
            java_files.append(Path(java_file).absolute())
    else:
        logging.error(f"{input_path} is neither a file nor a directory")
        sys.exit(-1)
    java_files = sorted(java_files, key=lambda path: str(path), reverse=False)
    return java_files


def get_decomposed_file(java_file: Path, brbo2_root: Path = Path(os.getcwd())) -> Path:
    inner_most_package_name = get_inner_most_package_name(java_file)
    decomposed_file_path = (
        brbo2_root / "output" / "decomposed" / inner_most_package_name
    )
    decomposed_file = decomposed_file_path / java_file.name
    return decomposed_file