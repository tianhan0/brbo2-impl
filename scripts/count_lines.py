import argparse
import logging
import scipy.stats as stats
import numpy
from common import print_args, get_files, configure_logging, to_csv

if __name__ == "__main__":
    """
    Usage: ~/brbo2-impl$ python3 scripts/count_lines.py --input src/main/java/brbo/benchmarks/sas22/
    """
    parser = argparse.ArgumentParser(
        description="Process data measurements from running brbo2 into tables in the paper."
    )
    parser.add_argument(
        "--input",
        type=str,
        required=True,
        help="The directory to search for java files.",
    )
    parser.set_defaults(dry=False)
    args = parser.parse_args()
    configure_logging(filename=None)
    print_args(args)

    dictionary = {}
    lines_of_code = []
    for java_file in get_files(path=args.input, prefix="", suffix="java"):
        file = open(java_file, "r")
        lines = file.readlines()
        dictionary.update({java_file.stem: [len(lines)]})
        lines_of_code.append(len(lines))
    interval = stats.norm.interval(
        confidence=0.95, loc=numpy.mean(lines_of_code), scale=stats.sem(lines_of_code)
    )
    logging.info(f"95% confidence interval is {interval}")
    print(to_csv(dictOrList=dictionary))
