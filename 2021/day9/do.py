#!/usr/bin/env python3

import numpy as np

toy_input = """2199943210
3987894921
9856789892
8767896789
9899965678"""

def local_minima(array2d):
    return ((array2d < np.roll(array2d,  1, 0)) &
            (array2d < np.roll(array2d, -1, 0)) &
            (array2d < np.roll(array2d,  1, 1)) &
            (array2d < np.roll(array2d, -1, 1)))

def find_basins(heights, minima):
    prev_basins, basins = None, minima
    while not np.array_equal(prev_basins, basins):
        new_basins = np.array(basins)

        # print(f"Current basins:\n {basins}")

        for shift, axis in [(1,0), (-1,0), (1,1), (-1,1)]:
            shifted_basins = np.roll(basins, shift, axis)
            shifted_heights = np.roll(heights, shift, axis)

            # print(f"{shift}, {axis}:")
            # print(f"{heights}\n")
            # print(f"{shifted_heights}\n")
            # print(f"{heights > shifted_heights}\n")
            # print(f"{shifted_basins}\n")
            # print(f"{shifted_basins & (heights > shifted_heights)}\n")

            new_basins |= (shifted_basins & (heights > shifted_heights) & (heights < 9))

            # print(f"{new_basins}")

        prev_basins = basins
        basins = new_basins

    return basins

def main():
    with open("./input.txt", "r") as f:
        inp = list(f.read().strip().splitlines())
    # inp = toy_input.splitlines()

    ns = [[int(n) for n in l] for l in inp]

    height_map: np.ndarray = np.array(ns, dtype='uint8')
    padded_heights = np.pad(height_map, pad_width=1, mode='constant', constant_values=10)

    padded_minima = local_minima(padded_heights)

    basins = []

    padded_minima_indices = np.argwhere(padded_minima)
    for padded_minimum_idx in padded_minima_indices:
        x,y = padded_minimum_idx
        only_that_minimum = np.zeros_like(padded_minima, dtype=bool)
        only_that_minimum[x,y] = True

        basins.append(find_basins(padded_heights, only_that_minimum))

    basins.sort(key = np.sum, reverse = True)

    unpadded_minima: np.ndarray = padded_minima[1:-1, 1:-1]

    low_points = height_map[unpadded_minima].flatten()
    risk_levels = low_points + 1

    print(f"Puzzle 1: {np.sum(risk_levels)}")
    print(f"Puzzle 2: {np.sum(basins[0])*np.sum(basins[1])*np.sum(basins[2])}")

    pass

if __name__ == "__main__": main()