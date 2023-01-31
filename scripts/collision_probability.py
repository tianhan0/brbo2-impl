import math
import itertools

# Compute the probability of two array elements to be the same,
# under the given array size and the given range of array elements
def collision_probability(array_element_range, array_size):
    no_collision_probability = (
        math.comb(array_element_range, array_size)  # Select k numbers from a range
        * math.factorial(array_size)  # Arrange the k numbers in any order
        / math.pow(array_element_range, array_size)  # All possible selections
    )
    return 1 - no_collision_probability


def subset_selection_complexity(set_size, max_select):
    sum = 0
    for select in range(1, max_select + 1):
        # print(f"{set_size} {select} {math.comb(set_size, select)}")
        sum = sum + math.comb(set_size, select)
    return sum


if __name__ == "__main__":
    ranges = [9, 50, 100, 1000]
    array_sizes = list(range(2, 15, 1))
    last_subset_selection = None
    for array_element_range, array_size in itertools.product(ranges, array_sizes):
        probability = collision_probability(
            array_element_range=array_element_range, array_size=array_size
        )
        subset_selection = subset_selection_complexity(
            set_size=array_size * 2, max_select=array_size
        )
        if last_subset_selection:
            subset_selection_multiplier = subset_selection / last_subset_selection
        else:
            subset_selection_multiplier = 0
        last_subset_selection = subset_selection
        print(
            f"Range {array_element_range}. Array size: {array_size}. Collision probability: {probability*100:.2f}%. Subset selection complexity: {subset_selection} (x{subset_selection_multiplier:.2f})."
        )
