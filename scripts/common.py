import logging
import sys
import json
import getpass
import time
import subprocess
import os
import resource
import re
from pathlib import Path

NO_DEPENDENCY_SCRIPT = "./scripts/run.sh"
WITH_DEPENDENCY_SCRIPT = "./scripts/run_deps.sh"


def configure_logging(filename=None):
    if filename:
        Path(filename).unlink(missing_ok=True)
        Path(filename).parent.mkdir(parents=True, exist_ok=True)
        logging.basicConfig(
            filename=filename,
            encoding="utf-8",
            level=logging.DEBUG,
            format="%(asctime)s %(levelname)-8s %(message)s",
        )
    else:
        logging.basicConfig(
            stream=sys.stdout,
            encoding="utf-8",
            level=logging.DEBUG,
            format="%(asctime)s %(levelname)-8s %(message)s",
        )


def _limit_memory():
    MAX_MEMORY = 6 * 1024 * 1024 * 1024  # 6 GB
    # The tuple below is of the form (soft limit, hard limit). Limit only
    # the soft part so that the limit can be increased later (setting also
    # the hard limit would prevent that).
    # When the limit cannot be changed, setrlimit() raises ValueError.
    resource.setrlimit(resource.RLIMIT_AS, (MAX_MEMORY, MAX_MEMORY))
    resource.setrlimit(resource.RLIMIT_DATA, (MAX_MEMORY, MAX_MEMORY))


# Return stdout and stderr, and the execution time
def run_command(command, cwd=os.getcwd(), dry=False, printOutput=True, timeout=None):
    logging.info(f"Under `{getpass.getuser()}@{cwd}`: Execute `{' '.join(command)}`")
    if dry:
        return "", 0
    if timeout:
        actual_command = [
            "timeout",
            "--preserve-status",
            "--kill-after",
            "3s",
            str(timeout),
        ]
        actual_command.extend(command)
    else:
        actual_command = command
    start_time = time.time()
    result = subprocess.run(
        actual_command,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        cwd=cwd,
        preexec_fn=_limit_memory,
    )
    execution_time = time.time() - start_time
    logging.info(f"Done. Execution time: {execution_time} seconds")
    if printOutput:
        logging.info(f"Output: {result.stdout}")
    return result.stdout.strip(), execution_time


def qfuzz_command(timeout, input, qfuzz, deps, mode, seed):
    command = [
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
    if mode == "naive":
        command.append("--naive")
    if seed:
        command.extend(["--input", str(seed)])
    return command


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
        # "--must-reset",
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
        self.total_verified = 0
        self.total_not_verified = 0
        self.total_unknown = 0

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
            self.total_verified = self.total_verified + 1
        elif "verified? No" in brbo_output:
            _increment_count(self.count_not_verified, inner_most_package_name)
            self.verification_results.update({str(java_file): "not verified"})
            self.total_not_verified = self.total_not_verified + 1
        else:
            _increment_count(self.count_unknown, inner_most_package_name)
            self.verification_results.update({str(java_file): "unknown"})
            self.total_unknown = self.total_unknown + 1

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

    def write(self, log_file):
        short_names = {
            file_name: str(Path(file_name).stem)
            for file_name in self.per_file_execution_time.keys()
        }
        output_contents = {
            "time_measurements": self.per_file_execution_time,
            "verification_results": self.verification_results,
            "short_names": short_names,
            "verified_programs": self.count_verified,
            "not_programs": self.count_not_verified,
            "unknown_programs": self.count_unknown,
            "total_time": self.total_time,
            "total_verified": self.total_verified,
            "total_not_verified": self.total_not_verified,
            "total_unknown": self.total_unknown,
        }
        with open(log_file, "w") as output_file:
            logging.info(f"Write into {log_file}")
            output_file.write(json.dumps(output_contents, indent=2))


def print_args(args):
    logging.info("Arguments:")
    for key, value in vars(args).items():
        logging.info(f"{key}\t{value}")


def get_files(path: str, prefix="", suffix=""):
    files = []
    input_path = Path(path)
    if input_path.is_file():
        files.append(input_path.absolute())
    elif input_path.is_dir():
        for java_file in input_path.rglob(f"{prefix}*.{suffix}"):
            files.append(Path(java_file).absolute())
    else:
        logging.error(f"{input_path} is neither a file nor a directory")
        sys.exit(-1)
    files = sorted(files, key=lambda path: str(path), reverse=False)
    return files


def get_decomposed_file(java_file: Path, brbo2_root: Path = Path(os.getcwd())) -> Path:
    inner_most_package_name = get_inner_most_package_name(java_file)
    decomposed_file_path = (
        brbo2_root / "output" / "decomposed" / inner_most_package_name
    )
    decomposed_file = decomposed_file_path / java_file.name
    return decomposed_file


def sbt_package(git_version, dry, cwd=os.getcwd()):
    pattern = re.compile("\d+")
    if pattern.match(git_version) is None:
        # This is probably a branch name, since it is not a sequence of numbers
        commit_hash, _ = run_command(
            command=["git", "log", '--format="%H"', "-n", "1"],
            printOutput=False,
            dry=dry,
        )
    else:
        commit_hash = git_version
    run_command(
        command=["git", "checkout", commit_hash], cwd=cwd, printOutput=False, dry=dry
    )
    logging.info(f"Build a new version: {commit_hash}")
    run_command(command=["sbt", "package"], cwd=cwd, dry=dry)
