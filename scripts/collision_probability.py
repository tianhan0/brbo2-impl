import math
import itertools

# Compute the probability of two array elements to be the same,
# under the given array size and the given range of array elements
def collision_probability(range, array_size):
    no_collision_probability = (
        math.comb(range, array_size)  # Select k numbers from a range
        * math.factorial(array_size)  # Arrange the k numbers in any order
        / math.pow(range, array_size)  # All possible selections
    )
    return 1 - no_collision_probability


if __name__ == "__main__":
    ranges = [10, 50, 100, 1000]
    array_sizes = list(range(2, 25))
    for range, array_size in itertools.product(ranges, array_sizes):
        probability = collision_probability(range=range, array_size=array_size)
        probability_string = "{:.2f}".format(probability * 100)
        print(
            f"Range {range}. Array size: {array_size}. Collision probability: {probability_string}%."
        )
