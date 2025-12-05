#!/usr/bin/env python3

from lib import *


def run(inp: Input):
    fresh_ranges, ids = inp.parsed

    fresh = IntervalTree()
    for s,e in fresh_ranges:
        fresh.addi(s, e+1)

    res = 0
    for id in ids:
        if fresh.overlaps_point(id):
            res += 1

    return res


def main():
    run_on_inputs(run, {1: 3})


if __name__ == "__main__":
    main()
