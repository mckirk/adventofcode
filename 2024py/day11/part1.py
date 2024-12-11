#!/usr/bin/env python3
from collections import defaultdict, Counter

from lib import *

sample1_expected = 55312
sample2_expected = None


def evolve(s):
    if s == 0:
        return [1]
    
    ss = str(s)
    if len(ss) % 2 == 0:
        half = len(ss) // 2
        return [int(ss[:half]), int(ss[half:])]
    
    return [s*2024]


def run(inp: Input):
    stones = inp.parsed
    for _ in range(25):
        new_stones = [evolve(s) for s in stones]
        stones = [item for sublist in new_stones for item in sublist]
    return len(stones)


def main():
    run_on_inputs(sample1_expected, sample2_expected, run)


if __name__ == "__main__":
    main()
