import argparse
import logging
import json
import scipy.stats as stats
import numpy
from common import print_args, get_files, pretty_print

# Assume the dictionary is a map from keys to a list of numbers
def _update(dictionary, file_name, new_data_point):
    existing = dictionary.get(file_name, [])
    existing.append(new_data_point)
    dictionary.update({file_name: existing})


# Assume the dictionary is a map from keys to a list of numbers
def transform_data(dictionary, mode):
    # https://www.geeksforgeeks.org/how-to-calculate-confidence-intervals-in-python/
    result = {}
    for key, value in dictionary.items():
        if mode == "confidence_interval":
            """
            new_value = stats.t.interval(
                confidence=0.95,
                df=len(value) - 1,
                loc=numpy.mean(value),
                scale=stats.sem(value),
            )
            """
            new_value = stats.norm.interval(
                confidence=0.95, loc=numpy.mean(value), scale=stats.sem(value)
            )
        elif mode == "mean":
            new_value = numpy.mean(value)
        else:
            raise AssertionError(f"Unknown mode: {mode}")
        result.update({key: new_value})
    return result


class Data:
    def __init__(self):
        self.total_fuzz_time = {}
        self.total_decompose_time = {}
        self.total_verification_time = {}
        self.total_verification_results = {}

    def insert_time_measurement(self, file_name, time_measurement):
        _update(
            dictionary=self.total_fuzz_time,
            file_name=file_name,
            new_data_point=time_measurement[0],
        )
        _update(
            dictionary=self.total_decompose_time,
            file_name=file_name,
            new_data_point=time_measurement[1],
        )
        _update(
            dictionary=self.total_verification_time,
            file_name=file_name,
            new_data_point=time_measurement[2],
        )

    def insert_verification_result(self, file_name, verification_result):
        if verification_result == "verified":
            verification_result = 1
        else:
            verification_result = 0
        _update(
            dictionary=self.total_verification_results,
            file_name=file_name,
            new_data_point=verification_result,
        )

    def print_raw(self):
        logging.info(f"Fuzz time:\n{pretty_print(self.total_fuzz_time)}")
        logging.info(f"Decompose time:\n{pretty_print(self.total_decompose_time)}")
        logging.info(
            f"Verification time:\n{pretty_print(self.total_verification_time)}"
        )
        logging.info(
            f"Verification results:\n{pretty_print(self.total_verification_results)}"
        )

    def transform_data(self):
        total_fuzz_time = transform_data(
            dictionary=self.total_fuzz_time, mode="confidence_interval"
        )
        total_decompose_time = transform_data(
            dictionary=self.total_decompose_time, mode="confidence_interval"
        )
        total_verification_time = transform_data(
            dictionary=self.total_verification_time, mode="confidence_interval"
        )
        total_verification_results = transform_data(
            dictionary=self.total_verification_results, mode="mean"
        )
        return (
            total_fuzz_time,
            total_decompose_time,
            total_verification_time,
            total_verification_results,
        )


if __name__ == "__main__":
    """
    Usage: ~/brbo2-impl$ python3 scripts/data.py \
      --input output/logs/verifiability/brbo2/ \
      --output output/logs/verifiability/brbo2.json \
      --mode verifiability
    """
    parser = argparse.ArgumentParser(
        description="Process data measurements from running brbo2 into tables in the paper."
    )
    parser.add_argument(
        "--input",
        type=str,
        required=True,
        help="The json file or the directory containing json files to be processed.",
    )
    parser.add_argument(
        "--output",
        type=str,
        required=True,
        help="The CSV file to write results to.",
    )
    parser.add_argument(
        "--mode",
        choices=["verifiability", "qfuzz", "timeout"],
        required=True,
        help="Which kind of table to generate.",
    )
    parser.set_defaults(dry=False)
    args = parser.parse_args()
    print_args(args)

    json_files = get_files(path=args.input, prefix="", suffix="json")
    data = Data()
    for json_file in json_files:
        with open(json_file, "r") as json_file:
            logging.info(f"Read from {args.input}")
            json_data = json.loads(json_file.read())
            time_measurements = json_data["time_measurements"]
            verification_results = json_data["verification_results"]

            for file_name, time_measurement in time_measurements.items():
                data.insert_time_measurement(
                    file_name=file_name, time_measurement=time_measurement
                )

            for file_name, verification_result in verification_results.items():
                data.insert_verification_result(
                    file_name=file_name, verification_result=verification_result
                )

    # data.print_raw()
    (
        total_fuzz_time,
        total_decompose_time,
        total_verification_time,
        total_verification_results,
    ) = data.transform_data()
    logging.info(f"Fuzz time: {pretty_print(total_fuzz_time)}")
    logging.info(f"Decompose time: {pretty_print(total_decompose_time)}")
    logging.info(f"Verification time: {pretty_print(total_verification_time)}")
    logging.info(f"Verification results: {pretty_print(total_verification_results)}")
    """
    json_object = json.dumps(output, indent=2)
    with open(args.output, "w") as output_file:
        print(f"Write into {args.output}")
        output_file.write(json_object)
    """
