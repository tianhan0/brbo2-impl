import argparse
import logging
import json
import scipy.stats as stats
import numpy
import csv
import tempfile
import os
import re
from pathlib import Path
from common import print_args, get_files, pretty_print, configure_logging

LOG = logging.getLogger(__name__)

# Assume the dictionary is a map from keys to a list of numbers
def _update(dictionary, file_name, new_data_point):
    existing = dictionary.get(file_name, [])
    existing.append(new_data_point)
    dictionary.update({file_name: existing})


# Assume the dictionary is a map from keys to a list of numbers
def _transform_data(dictionary, mode):
    # https://www.geeksforgeeks.org/how-to-calculate-confidence-intervals-in-python/
    result = {}
    for key, values in dictionary.items():
        # Sometimes the values are missing
        values = [item if item else 0 for item in values]
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
                confidence=0.95, loc=numpy.mean(values), scale=stats.sem(values)
            )
        elif mode == "mean":
            new_value = [numpy.mean(values)]
        else:
            raise AssertionError(f"Unknown mode: {mode}")
        # Return a list here, such that all dictionaries are of the same shape: str -> list of numbers
        # Such that we can uniformly work on the dictionaries (e.g., printing into csv)
        new_value = list(new_value)
        # We should not see negative numbers for any of our measurement
        new_value = [item if item >= 0 else 0 for item in new_value]
        result.update({key: new_value})
    return result


def _simplify_filename(file_name: str):
    return Path(file_name).stem


def _to_csv(dictOrList):
    with tempfile.NamedTemporaryFile() as csv_file:
        with open(csv_file.name, "w") as csv_file:
            writer = csv.writer(csv_file)
            if type(dictOrList) is dict:
                for file_name, measurements in dictOrList.items():
                    measurements = [
                        f"{measurement:.2f}" for measurement in measurements
                    ]
                    writer.writerow([file_name, *measurements])
            elif type(dictOrList) is list:
                writer.writerow(dictOrList)
        with open(csv_file.name, "r") as csv_file:
            return csv_file.read().strip()


class Data:
    def __init__(self):
        self.total_fuzz_time = {}
        self.total_decompose_time = {}
        self.total_verification_time = {}
        self.total_verification_results = {}
        self.total_trace_clusters = {}
        self.total_decision_tree_predicate_counts = {}

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

    def insert_trace_cluster(self, file_name, trace_cluster):
        _update(
            dictionary=self.total_trace_clusters,
            file_name=file_name,
            new_data_point=trace_cluster,
        )

    def insert_decision_tree_predicate_count(
        self, file_name, decision_tree_predicate_count
    ):
        _update(
            dictionary=self.total_decision_tree_predicate_counts,
            file_name=file_name,
            new_data_point=decision_tree_predicate_count,
        )

    def print_raw(self):
        LOG.info(f"Fuzz time:\n{pretty_print(self.total_fuzz_time)}")
        LOG.info(f"Decompose time:\n{pretty_print(self.total_decompose_time)}")
        LOG.info(f"Verification time:\n{pretty_print(self.total_verification_time)}")
        LOG.info(
            f"Verification results:\n{pretty_print(self.total_verification_results)}"
        )

    def transform_data(self):
        total_fuzz_time = _transform_data(
            dictionary=self.total_fuzz_time, mode="mean"
        )
        total_decompose_time = _transform_data(
            dictionary=self.total_decompose_time, mode="mean"
        )
        total_verification_time = _transform_data(
            dictionary=self.total_verification_time, mode="mean"
        )
        total_verification_results = _transform_data(
            dictionary=self.total_verification_results,
            mode="mean"
            # dictionary=self.total_verification_results, mode="confidence_interval"
        )
        total_trace_clusters = _transform_data(
            dictionary=self.total_trace_clusters, mode="mean"
        )
        total_decision_tree_predicate_counts = _transform_data(
            dictionary=self.total_decision_tree_predicate_counts, mode="mean"
        )
        return (
            total_fuzz_time,
            total_decompose_time,
            total_verification_time,
            total_verification_results,
            total_trace_clusters,
            total_decision_tree_predicate_counts,
        )

    def pretty_print(self):
        (
            total_fuzz_time,
            total_decompose_time,
            total_verification_time,
            total_verification_results,
            total_trace_clusters,
            decision_tree_predicate_counts,
        ) = self.transform_data()
        LOG.info(f"Fuzz time: {pretty_print(total_fuzz_time)}")
        LOG.info(f"Decompose time: {pretty_print(total_decompose_time)}")
        LOG.info(f"Verification time: {pretty_print(total_verification_time)}")
        LOG.info(f"Verification results: {pretty_print(total_verification_results)}")
        LOG.info(f"Trace clusters: {pretty_print(total_trace_clusters)}")
        LOG.info(
            f"Decision tree predicate counts: {pretty_print(decision_tree_predicate_counts)}"
        )


def _keep_n_latest(files, latest):
    sorted_files = sorted(files, key=lambda file: os.path.getctime(file), reverse=True)
    if latest:
        return sorted_files[:latest]
    else:
        return sorted_files


def _latest_log_files(log_directory, prefix, suffix, latest):
    logs = get_files(path=log_directory, prefix=prefix, suffix=suffix)
    return _keep_n_latest(files=logs, latest=latest)


def _get_json_files(mode, input_directory, latest):
    if mode == "qfuzz":
        naive_logs = _latest_log_files(
            log_directory=input_directory, prefix="naive", suffix="json", latest=latest
        )
        qfuzz_logs = _latest_log_files(
            log_directory=input_directory, prefix="qfuzz", suffix="json", latest=latest
        )
        return {"naive": naive_logs, "qfuzz": qfuzz_logs}
    elif mode == "timeout":
        log_files = _latest_log_files(
            log_directory=input_directory,
            prefix="timeout",
            suffix="json",
            latest=latest,
        )
        prefixes = []
        for log_file in log_files:
            prefixes.extend(re.findall(r"timeout\d+", str(log_file)))
        prefixes = list(set(prefixes))
        prefixes.sort(reverse=False)
        return {
            prefix: _latest_log_files(
                log_directory=input_directory,
                prefix=prefix,
                suffix="json",
                latest=latest,
            )
            for prefix in prefixes
        }
    elif mode == "verifiability":
        worst_logs = _latest_log_files(
            log_directory=input_directory, prefix="worst", suffix="json", latest=latest
        )
        fully_logs = _latest_log_files(
            log_directory=input_directory, prefix="fully", suffix="json", latest=latest
        )
        select_logs = _latest_log_files(
            log_directory=input_directory, prefix="select", suffix="json", latest=latest
        )
        return {"worst": worst_logs, "fully": fully_logs, "select": select_logs}


def _build_csv_header(log_names):
    csv_header = ["program names"]
    for log_name in log_names:
        csv_header.append(f"fuzz time ({log_name})")
        # csv_header.append("")
    for log_name in log_names:
        csv_header.append(f"decompose time ({log_name})")
        # csv_header.append("")
    for log_name in log_names:
        csv_header.append(f"verification time ({log_name})")
        # csv_header.append("")
    for log_name in log_names:
        csv_header.append(f"verification results ({log_name})")
    for log_name in log_names:
        csv_header.append(f"trace clusters ({log_name})")
    for log_name in log_names:
        csv_header.append(f"decision tree predicates ({log_name})")
    return csv_header


if __name__ == "__main__":
    """
    Usage: ~/brbo2-impl$ python3 scripts/data.py \
      --input logs/qfuzz/ \
      --mode qfuzz
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
        "--latest",
        type=int,
        default=None,
        help="The latest N log files to process, based on their file creation time.",
    )
    # parser.add_argument(
    #     "--output",
    #     type=str,
    #     required=True,
    #     help="The CSV file to write results to.",
    # )
    parser.add_argument(
        "--mode",
        choices=["verifiability", "qfuzz", "timeout"],
        required=True,
        help="Which kind of table to generate.",
    )
    parser.set_defaults(dry=False)
    args = parser.parse_args()
    configure_logging(filename=None)
    print_args(args)

    # A map from file names to the measurements under different configurations
    table = {}
    measurements = {}
    file_names = set()
    log_files = _get_json_files(
        mode=args.mode, input_directory=args.input, latest=args.latest
    )
    for log_name, json_files in log_files.items():
        LOG.info(f"Summarize logs for `{log_name}` under mode `{args.mode}`")
        data = Data()
        for json_file in json_files:
            with open(json_file, "r") as file:
                LOG.info(f"Read from {json_file}")
                json_data = json.loads(file.read())
                time_measurements = json_data["time_measurements"]
                verification_results = json_data["verification_results"]
                trace_clusters = (
                    json_data["trace_clusters"] if "trace_clusters" in json_data else {}
                )
                decision_tree_predicate_counts = (
                    json_data["decision_tree_predicate_count"]
                    if "decision_tree_predicate_count" in json_data
                    else {}
                )

                for file_name, time_measurement in time_measurements.items():
                    data.insert_time_measurement(
                        file_name=file_name, time_measurement=time_measurement
                    )
                    file_names.add(file_name)

                for file_name, verification_result in verification_results.items():
                    data.insert_verification_result(
                        file_name=file_name, verification_result=verification_result
                    )

                for file_name, trace_cluster in trace_clusters.items():
                    data.insert_trace_cluster(
                        file_name=file_name, trace_cluster=trace_cluster
                    )

                for (
                    file_name,
                    decision_tree_predicate_count,
                ) in decision_tree_predicate_counts.items():
                    data.insert_decision_tree_predicate_count(
                        file_name=file_name,
                        decision_tree_predicate_count=decision_tree_predicate_count,
                    )
        measurements.update({log_name: data})

    csv_header = _build_csv_header(log_names=measurements.keys())
    for file_name in sorted(file_names):
        fuzz_time = []
        decompose_time = []
        verification_time = []
        verification_results = []
        trace_clusters = []
        decision_tree_predicate_counts = []
        for log_name, measurement in measurements.items():
            (
                total_fuzz_time,
                total_decompose_time,
                total_verification_time,
                total_verification_results,
                total_trace_clusters,
                total_decision_tree_predicate_counts,
            ) = measurement.transform_data()
            fuzz_time_entry = total_fuzz_time.get(file_name, [])
            fuzz_time.extend(fuzz_time_entry)

            decompose_time_entry = total_decompose_time.get(file_name, [])
            decompose_time.extend(decompose_time_entry)

            verification_time_entry = total_verification_time.get(file_name, [])
            verification_time.extend(verification_time_entry)

            verification_result_entry = total_verification_results.get(file_name, [])
            verification_results.extend(verification_result_entry)

            trace_cluster_entry = total_trace_clusters.get(file_name, [])
            trace_clusters.extend(trace_cluster_entry)

            decision_tree_predicate_count_entry = (
                total_decision_tree_predicate_counts.get(file_name, [])
            )
            decision_tree_predicate_counts.extend(decision_tree_predicate_count_entry)
        table.update(
            {
                _simplify_filename(file_name): [
                    *fuzz_time,
                    *decompose_time,
                    *verification_time,
                    *verification_results,
                    *trace_clusters,
                    *decision_tree_predicate_counts,
                ]
            }
        )
    LOG.info("Summarize the logs into a CSV table:")
    print(_to_csv(dictOrList=csv_header))
    print(_to_csv(dictOrList=table))

    """
    json_object = json.dumps(output, indent=2)
    with open(args.output, "w") as output_file:
        print(f"Write into {args.output}")
        output_file.write(json_object)
    """
