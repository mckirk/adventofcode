#!/usr/bin/env python3
from collections import defaultdict, Counter

from lib import *

sample1_expected = 81
sample2_expected = 227


dirs = [Dir.N, Dir.E, Dir.S, Dir.W]


def run(inp: Input):
    d = {V(x, y): int(c) for (x, y), c in inp.as_pos if c != "."}
    starts = {(p, c) for p, c in d.items() if c == 0}

    res = 0
    for x in starts:
        reachable = set()
        cur = [x]

        while cur:
            p, c = cur.pop()
            for dir in dirs:
                if d.get(p+dir.value) == c+1:
                    if c+1 == 9:
                        res += 1
                    else:
                        cur.append((p+dir.value, c+1))

        # res += len(reachable)
    
    return res


def main():
    run_on_inputs(sample1_expected, sample2_expected, run)


if __name__ == "__main__":
    main()
