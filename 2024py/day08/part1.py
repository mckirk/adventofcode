#!/usr/bin/env python3
from collections import defaultdict, Counter

from lib import *

sample1_expected = 14
sample2_expected = None


def run(inp: Input):
    pos_by_c: dict[str, set[V]] = defaultdict(set)

    antinodes = set()

    for (x, y), c in inp.as_pos:
        if c == ".":
            continue
        p = V(x, y)
        prev = pos_by_c[c]

        for pr in prev:
            delta = p - pr
            a1 = p + delta
            a2 = pr - delta

            if a1.within(inp.limits):
                antinodes.add(a1)

            if a2.within(inp.limits):
                antinodes.add(a2)

        pos_by_c[c].add(p)

    return len(antinodes)


def main():
    run_on_inputs(sample1_expected, sample2_expected, run)


if __name__ == "__main__":
    main()
