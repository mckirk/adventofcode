#!/usr/bin/env python3

from lib import *


def run(inp: Input):
    fresh_ranges, ids = inp.parsed

    fresh = IntervalTree()
    for s,e in fresh_ranges:
        fresh.addi(s, e+1)

    fresh.merge_overlaps()

    res = 0
    for interval in fresh.items():
        res += interval.length()

    return res


def main():
    run_on_inputs(run, {1: 14})


if __name__ == "__main__":
    main()
