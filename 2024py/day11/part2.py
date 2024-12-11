#!/usr/bin/env python3
from collections import defaultdict, Counter
from functools import cache

from lib import *

sample1_expected = None
sample2_expected = None


@cache
def evolve(s):
    if s == 0:
        return (1,)
    
    ss = str(s)
    if len(ss) % 2 == 0:
        half = len(ss) // 2
        return (int(ss[:half]), int(ss[half:]))
    
    return (s*2024,)


@cache
def evolve_n(stones, n):
    if n == 0:
        return len(stones)
    
    return sum(evolve_n(evolve(s), n-1) for s in stones)


def run(inp: Input):
    stones = tuple(inp.parsed)
    return evolve_n(stones, 75)


def main():
    run_on_inputs(sample1_expected, sample2_expected, run)


if __name__ == "__main__":
    main()
