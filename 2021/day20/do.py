#!/usr/bin/env python3

import numpy as np
from collections import defaultdict
import re

from scipy import signal

toy_input = """..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..## #..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.### .######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#. .#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#..... .#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.. ...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#..... ..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###"""

lookup_kernel = np.array([
    [2**0, 2**1, 2**2],
    [2**3, 2**4, 2**5],
    [2**6, 2**7, 2**8]])


def enhance(img, algo, pad=0):
    padded = np.pad(img, 0)

    indices = signal.convolve2d(padded, lookup_kernel, fillvalue=pad)

    algoed = algo[indices]

    return algoed

def enhance_much(img, algo, times, switch_boundary):
    boundary = 0

    cur = img
    for i in range(times):
        cur = enhance(cur, algo, boundary)
        if switch_boundary:
            boundary ^= 1

    return cur

def print_img(img):
    print("\n".join("".join(".#"[b] for b in l) for l in img))

def main():
    with open("./input.txt", "r") as f:
       inp = list(f.read().strip().splitlines())
    # inp = toy_input.splitlines()

    algo = np.array([c == '#' for c in inp[0]], dtype=bool)
    img = np.array([[c == '#' for c in l] for l in inp[2:]], dtype=bool)

    print(f"Puzzle1: {np.sum(enhance_much(img, algo, 2, switch_boundary=True))}")
    print(f"Puzzle2: {np.sum(enhance_much(img, algo, 50, switch_boundary=True))}")

    pass

if __name__ == "__main__": main()