import itertools
import math


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
            f"Range {array_element_range}. Array size: {array_size}. Collision probability: {probability * 100:.2f}%. Subset selection complexity: {subset_selection} (x{subset_selection_multiplier:.2f})."
        )

"""
Example Output:
Range 9. Array size: 2. Collision probability: 11.11%. Subset selection complexity: 10 (x0.00).
Range 9. Array size: 3. Collision probability: 30.86%. Subset selection complexity: 41 (x4.10).
Range 9. Array size: 4. Collision probability: 53.91%. Subset selection complexity: 162 (x3.95).
Range 9. Array size: 5. Collision probability: 74.39%. Subset selection complexity: 637 (x3.93).
Range 9. Array size: 6. Collision probability: 88.62%. Subset selection complexity: 2509 (x3.94).
Range 9. Array size: 7. Collision probability: 96.21%. Subset selection complexity: 9907 (x3.95).
Range 9. Array size: 8. Collision probability: 99.16%. Subset selection complexity: 39202 (x3.96).
Range 9. Array size: 9. Collision probability: 99.91%. Subset selection complexity: 155381 (x3.96).
Range 9. Array size: 10. Collision probability: 100.00%. Subset selection complexity: 616665 (x3.97).
Range 9. Array size: 11. Collision probability: 100.00%. Subset selection complexity: 2449867 (x3.97).
Range 9. Array size: 12. Collision probability: 100.00%. Subset selection complexity: 9740685 (x3.98).
Range 9. Array size: 13. Collision probability: 100.00%. Subset selection complexity: 38754731 (x3.98).
Range 9. Array size: 14. Collision probability: 100.00%. Subset selection complexity: 154276027 (x3.98).
Range 50. Array size: 2. Collision probability: 2.00%. Subset selection complexity: 10 (x0.00).
Range 50. Array size: 3. Collision probability: 5.92%. Subset selection complexity: 41 (x4.10).
Range 50. Array size: 4. Collision probability: 11.56%. Subset selection complexity: 162 (x3.95).
Range 50. Array size: 5. Collision probability: 18.64%. Subset selection complexity: 637 (x3.93).
Range 50. Array size: 6. Collision probability: 26.78%. Subset selection complexity: 2509 (x3.94).
Range 50. Array size: 7. Collision probability: 35.56%. Subset selection complexity: 9907 (x3.95).
Range 50. Array size: 8. Collision probability: 44.58%. Subset selection complexity: 39202 (x3.96).
Range 50. Array size: 9. Collision probability: 53.45%. Subset selection complexity: 155381 (x3.96).
Range 50. Array size: 10. Collision probability: 61.83%. Subset selection complexity: 616665 (x3.97).
Range 50. Array size: 11. Collision probability: 69.46%. Subset selection complexity: 2449867 (x3.97).
Range 50. Array size: 12. Collision probability: 76.18%. Subset selection complexity: 9740685 (x3.98).
Range 50. Array size: 13. Collision probability: 81.90%. Subset selection complexity: 38754731 (x3.98).
Range 50. Array size: 14. Collision probability: 86.60%. Subset selection complexity: 154276027 (x3.98).
Range 100. Array size: 2. Collision probability: 1.00%. Subset selection complexity: 10 (x0.00).
Range 100. Array size: 3. Collision probability: 2.98%. Subset selection complexity: 41 (x4.10).
Range 100. Array size: 4. Collision probability: 5.89%. Subset selection complexity: 162 (x3.95).
Range 100. Array size: 5. Collision probability: 9.65%. Subset selection complexity: 637 (x3.93).
Range 100. Array size: 6. Collision probability: 14.17%. Subset selection complexity: 2509 (x3.94).
Range 100. Array size: 7. Collision probability: 19.32%. Subset selection complexity: 9907 (x3.95).
Range 100. Array size: 8. Collision probability: 24.97%. Subset selection complexity: 39202 (x3.96).
Range 100. Array size: 9. Collision probability: 30.97%. Subset selection complexity: 155381 (x3.96).
Range 100. Array size: 10. Collision probability: 37.18%. Subset selection complexity: 616665 (x3.97).
Range 100. Array size: 11. Collision probability: 43.47%. Subset selection complexity: 2449867 (x3.97).
Range 100. Array size: 12. Collision probability: 49.68%. Subset selection complexity: 9740685 (x3.98).
Range 100. Array size: 13. Collision probability: 55.72%. Subset selection complexity: 38754731 (x3.98).
Range 100. Array size: 14. Collision probability: 61.48%. Subset selection complexity: 154276027 (x3.98).
Range 1000. Array size: 2. Collision probability: 0.10%. Subset selection complexity: 10 (x0.00).
Range 1000. Array size: 3. Collision probability: 0.30%. Subset selection complexity: 41 (x4.10).
Range 1000. Array size: 4. Collision probability: 0.60%. Subset selection complexity: 162 (x3.95).
Range 1000. Array size: 5. Collision probability: 1.00%. Subset selection complexity: 637 (x3.93).
Range 1000. Array size: 6. Collision probability: 1.49%. Subset selection complexity: 2509 (x3.94).
Range 1000. Array size: 7. Collision probability: 2.08%. Subset selection complexity: 9907 (x3.95).
Range 1000. Array size: 8. Collision probability: 2.77%. Subset selection complexity: 39202 (x3.96).
Range 1000. Array size: 9. Collision probability: 3.55%. Subset selection complexity: 155381 (x3.96).
Range 1000. Array size: 10. Collision probability: 4.41%. Subset selection complexity: 616665 (x3.97).
Range 1000. Array size: 11. Collision probability: 5.37%. Subset selection complexity: 2449867 (x3.97).
Range 1000. Array size: 12. Collision probability: 6.41%. Subset selection complexity: 9740685 (x3.98).
Range 1000. Array size: 13. Collision probability: 7.53%. Subset selection complexity: 38754731 (x3.98).
Range 1000. Array size: 14. Collision probability: 8.74%. Subset selection complexity: 154276027 (x3.98).
"""
