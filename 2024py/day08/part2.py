#!/usr/bin/env python3
from collections import defaultdict, Counter

from lib import *

sample1_expected = 34
sample2_expected = 9


def run(inp: Input):
    pos_by_c: dict[str, set[V]] = defaultdict(set)

    limits = (len(inp.lines[0]), len(inp.lines))

    antinodes = set()

    for y, l in enumerate(inp.lines):
        for x, c in enumerate(l):
            if c == ".":
                continue

            p = V(x, y)
            prev = pos_by_c[c]

            for pr in prev:
                delta = p - pr
                for i in range(100):
                    a1 = p + i*delta
                    a2 = pr - i*delta

                    if a1.within(limits):
                        antinodes.add(a1)

                    if a2.within(limits):
                        antinodes.add(a2)
                
            pos_by_c[c].add(p)

    return len(antinodes)

def main():
    run_on_inputs(sample1_expected, sample2_expected, run)


if __name__ == "__main__":
    main()
