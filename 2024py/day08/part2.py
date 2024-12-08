#!/usr/bin/env python3
from collections import defaultdict, Counter
from itertools import count

from lib import *

sample1_expected = 34
sample2_expected = 9


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
            for i in count():
                a1 = p + i * delta
                a2 = pr - i * delta

                cont = False
                if a1.within(inp.limits):
                    antinodes.add(a1)
                    cont = True

                if a2.within(inp.limits):
                    antinodes.add(a2)
                    cont = True

                if not cont:
                    break

        pos_by_c[c].add(p)

    return len(antinodes)


def main():
    run_on_inputs(sample1_expected, sample2_expected, run)


if __name__ == "__main__":
    main()
