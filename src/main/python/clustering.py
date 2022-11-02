from sklearn.cluster import OPTICS, KMeans
from sklearn.neighbors import NearestNeighbors
import numpy
import argparse
import json


def optics(data, max_eps):
    print(f"Cluster algorithm: OPTICS")
    clustering = OPTICS(
        min_samples=2,
        max_eps=max_eps,
        metric="precomputed",
        cluster_method="xi",
        min_cluster_size=1,
    ).fit(data)
    print(f"Ordering: {clustering.ordering_}")
    print(f"Reachability: {clustering.reachability_[clustering.ordering_]}")
    print(f"Labels: {clustering.labels_}")
    # print(clustering.cluster_hierarchy_)
    return clustering.labels_.tolist()


def k_means(data, n_clusters):
    print(f"Cluster algorithm: KMeans")
    if n_clusters:
        kmeans = KMeans(n_clusters=n_clusters, random_state=0)
    else:
        kmeans = KMeans(random_state=0)
    kmeans.fit(data)
    print(f"Labels: {kmeans.labels_}")
    return kmeans.labels_.tolist()


def knn(data):
    neighbors = NearestNeighbors(n_neighbors=1, metric="precomputed").fit(data)
    distances, indices = neighbors.kneighbors(data, return_distance=True)
    print(f"Distances:\n{distances}")
    print(f"Indices:\n{indices}")
    print(f"Neighbors graph:\n{neighbors.kneighbors_graph(data)}")
    print(f"Radius neighbors graph:\n{neighbors.radius_neighbors_graph(data)}")
    # TODO: Return labels
    return


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Parse arguments for clustering.")
    parser.add_argument(
        "--algorithm",
        type=str,
        default="optics",
        choices=["optics", "kmeans", "knn"],
        help="The output json file",
    )
    parser.add_argument(
        "--input", type=str, required=True, help="The input json file to cluster"
    )
    parser.add_argument(
        "--output", type=str, required=True, help="The output json file"
    )
    parser.add_argument(
        "--max-eps",
        type=float,
        default=numpy.inf,
        help="OPTICS algorithm: The maximum distance between two samples for one to be considered as in the neighborhood of the other.",
    )
    parser.add_argument(
        "--clusters",
        type=int,
        help="K-Means: The number of clusters to form as well as the number of centroids to generate.",
    )
    args = parser.parse_args()

    with open(args.input, "r") as input_file:
        print(f"Read from {args.input}")
        json_data = json.loads(input_file.read())
        distance_matrix = json_data["data"]
        print(f"Data matrix: {distance_matrix}")

        data = numpy.array(distance_matrix, dtype=object)
        if args.algorithm == "optics":
            labels = optics(data, max_eps=args.max_eps)
        elif args.algorithm == "knn":
            labels = knn(data)
        elif args.algorithm == "kmeans":
            labels = k_means(data, args.clusters)
        else:
            print(f"Unknown algorithm {args.algorithm}")
            sys.exit(-1)

        output_json = {"data": distance_matrix, "labels": labels}
        json_object = json.dumps(output_json, indent=2)
        with open(args.output, "w") as output_file:
            print(f"Write into {args.output}")
            output_file.write(json_object)
