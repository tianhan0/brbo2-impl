import argparse
import subprocess
import glob
from pathlib import Path


def run_command(command, cwd):
    print(f"Execute command {command} under {cwd}")
    result = subprocess.run(
        command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, cwd=cwd
    )
    print(f"Output: {result.stdout}")


def qfuzz_command(timeout, input):
    return [
        "./scripts/run.sh",
        "fuzz",
        "--timeout",
        f"{timeout}",
        "--directory",
        f"{input}",
        "-o",
        "src/main/java/brbo/fuzz/",
        "--dry",
        "--qfuzz",
        Path("~/Documents/workspace/qfuzz_docker/").expanduser(),
    ]


def decomposition_command(threads, samples, input):
    return [
        "./scripts/run.sh",
        "decompose",
        "--threads",
        f"{threads}",
        "--debug",
        "--algorithm",
        "optics",
        "--parameter",
        f"{0.1}",
        "--samples",
        f"{samples}",
        "--directory",
        f"{input}",
    ]


def verification_command(decomposed_file):
    return [
        "./scripts/run_without_deps.sh",
        "--directory",
        decomposed_file,
        "--amortize",
        "transparent",
        "--icra-path",
        Path("~/Documents/workspace/icra/icra").expanduser(),
    ]


if __name__ == "__main__":
    # Usage: ~/brbo2-impl$ python3 scripts/fuzz+decompose.py  --input src/main/java/brbo/benchmarks/sas22/stac/TemplateEngine.java
    parser = argparse.ArgumentParser(
        description="Parse bytes from a file into a sequence of shorts."
    )
    parser.add_argument(
        "--timeout",
        type=int,
        required=False,
        default=3,
        help="The timeout for running AFL in QFuzz (in seconds).",
    )
    parser.add_argument(
        "--input", type=str, required=True, help="The file or the directory to analyze."
    )
    parser.add_argument(
        "--threads",
        type=int,
        required=False,
        default=6,
        help="The number of threads when decomposing a program.",
    )
    parser.add_argument(
        "--samples",
        type=int,
        default=10,
        help="The number of samples when randomly generating inputs for decomposing a program.",
    )
    parser.add_argument(
        "--brbo2", type=str, default="~/brbo2-impl", help="The directory of brbo2-impl."
    )
    parser.add_argument(
        "--brbo", type=str, default="~/brbo-impl", help="The directory of brbo-impl."
    )
    args = parser.parse_args()

    java_files = []
    input_path = Path(args.input)
    if input_path.is_file():
        java_files.append(input_path.absolute())
    elif input_path.is_dir():
        for java_file in glob.iglob(args.input + "**/*.java", recursive=True):
            java_files.append(Path(java_file).absolute())
    else:
        print(f"{input_path} is neither a file nor a directory")
        sys.exit(-1)

    brbo2_root = Path(args.brbo2).expanduser()
    brbo_root = Path(args.brbo).expanduser()

    run_qfuzz = qfuzz_command(timeout=args.timeout, input=args.input)
    run_decomposition = decomposition_command(
        threads=args.threads, samples=args.samples, input=args.input
    )

    for java_file in java_files:
        print(f"Process file `{java_file}`")

        run_command(command=run_qfuzz, cwd=brbo2_root)

        run_command(command=run_decomposition, cwd=brbo2_root)

        parts = java_file.parent.parts
        decomposed_file = (
            brbo2_root / "output" / "decomposed" / parts[-1] / java_file.name
        )
        run_verification = verification_command(decomposed_file=decomposed_file)
        run_command(command=run_verification, cwd=brbo_root)
