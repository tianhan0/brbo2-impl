from sklearn import tree
import numpy
import argparse
import json
import matplotlib


def classify(features, labels, print_tree):
    classifier = tree.DecisionTreeClassifier()
    classifier = classifier.fit(features, labels)
    if print_tree:
        print(f"Tree:\n{tree.export_text(classifier)}")
        # show(classifier)
    return traverse(classifier, print_tree=False)


def show(classifier):
    tree.plot_tree(classifier)
    matplotlib.pyplot.show()


def traverse(classifier, print_tree):
    n_nodes = classifier.tree_.node_count
    children_left = classifier.tree_.children_left
    children_right = classifier.tree_.children_right
    feature = classifier.tree_.feature
    threshold = classifier.tree_.threshold
    value = classifier.tree_.value

    node_depth = numpy.zeros(shape=n_nodes, dtype=numpy.int64)
    is_leaves = numpy.zeros(shape=n_nodes, dtype=bool)
    stack = [(0, 0)]  # start with the root node id (0) and its depth (0)
    while len(stack) > 0:
        # `pop` ensures each node is only visited once
        node_id, depth = stack.pop()
        node_depth[node_id] = depth

        # If the left and right child of a node is not the same we have a split
        # node
        is_split_node = children_left[node_id] != children_right[node_id]
        # If a split node, append left and right children and depth to `stack`
        # so we can loop through them
        if is_split_node:
            stack.append((children_left[node_id], depth + 1))
            stack.append((children_right[node_id], depth + 1))
        else:
            is_leaves[node_id] = True

    if print_tree:
        print(
            "The binary tree structure has {n} nodes and has "
            "the following tree structure:\n".format(n=n_nodes)
        )
    leaves = []
    non_leaves = []
    for i in range(n_nodes):
        if is_leaves[i]:
            if print_tree:
                print(
                    "{space}node={node} is a leaf node.".format(
                        space=node_depth[i] * "\t", node=i
                    )
                )
            samples = value[i].tolist()
            non_empty_sample_index = -1
            for index, sample in enumerate(samples[0]):
                if sample > 0:
                    non_empty_sample_index = index
            assert non_empty_sample_index != -1
            leaves.append({"nodeID": i, "classID": non_empty_sample_index})
        else:
            if print_tree:
                print(
                    "{space}node={node} is a split node: "
                    "go to node {left} if X[:, {feature}] <= {threshold} "
                    "else to node {right}.".format(
                        space=node_depth[i] * "\t",
                        node=i,
                        left=children_left[i],
                        feature=feature[i],
                        threshold=threshold[i],
                        right=children_right[i],
                    )
                )
            non_leaves.append(
                {
                    "nodeID": i,
                    "leftNodeID": int(children_left[i]),
                    "rightNodeID": int(children_right[i]),
                    "threshold": threshold[i],
                    "featureID": int(feature[i]),
                }
            )
    classes = map(lambda x: str(x), classifier.classes_.tolist())
    return {"leaves": leaves, "non_leaves": non_leaves, "classes": list(classes)}


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Parse arguments for clustering.")
    parser.add_argument(
        "--input", type=str, required=True, help="The input json file to cluster"
    )
    parser.add_argument(
        "--output", type=str, required=True, help="The output json file"
    )
    parser.add_argument(
        "--print-tree", type=bool, default=False, help="Whether to print the tree"
    )
    args = parser.parse_args()

    with open(args.input, "r") as input_file:
        print(f"Read from {args.input}")
        json_data = json.loads(input_file.read())
        features = json_data["features"]
        labels = json_data["labels"]

        features = numpy.array(features, dtype=object)
        print(f"Features:\n{features}")
        print(f"Labels: {labels}")
        output = classify(features, labels, print_tree=args.print_tree)

        json_object = json.dumps(output, indent=2)
        with open(args.output, "w") as output_file:
            print(f"Write into {args.output}")
            output_file.write(json_object)
