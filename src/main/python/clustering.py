from sklearn.cluster import OPTICS, KMeans
import numpy
import argparse
import json


def optics(data, max_eps):
    clustering = OPTICS(min_samples=2, max_eps=max_eps, metric="precomputed").fit(data)
    return clustering.labels_
    # print(clustering.cluster_hierarchy_)


def k_means(data):
    kmeans = KMeans(n_clusters=3, random_state=0).fit(data)
    print(kmeans.labels_)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Parse arguments for clustering.")
    parser.add_argument("--input", type=str, help="The input json file to cluster")
    parser.add_argument("--output", type=str, help="The output json file")
    parser.add_argument(
        "--max-eps",
        type=float,
        default=numpy.inf,
        help="The maximum distance between two samples for one to be considered as in the neighborhood of the other.",
    )
    args = parser.parse_args()

    with open(args.input, "r") as input_file:
        print(f"Read from {args.input}")
        json_data = json.loads(input_file.read())
        distance_matrix = json_data["data"]
        print(f"Distance matrix: {distance_matrix}")

        data = numpy.array(distance_matrix, dtype=object)
        labels = optics(data, max_eps=args.max_eps)

        output_json = {"data": distance_matrix, "labels": labels.tolist()}
        json_object = json.dumps(output_json, indent=2)
        with open(args.output, "w") as output_file:
            print(f"Write into {args.output}")
            output_file.write(json_object)
