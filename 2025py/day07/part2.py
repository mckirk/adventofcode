#!/usr/bin/env python3

from collections import Counter
from lib import *


def run(inp: Input):
    grid = {p: c for p, c in inp.as_pos}

    grid_inv = defaultdict(list)
    for p, c in grid.items():
        grid_inv[c].append(p)

    limits = get_limits(inp.lines)

    start = grid_inv["S"][0]
    splitters = set(grid_inv["^"])

    cur = Counter()
    cur[start] = 1
    next = Counter()

    for _ in range(limits[0]-1):
        def add_next(p: V, c: int):
            if p.within(limits):
                next[p] += c
        
        for p, count in cur.items():
            np = p + V(0, 1)
            if np in splitters:
                add_next(np + (V( 1, 0)), count)
                add_next(np + (V(-1, 0)), count)
            else:
                add_next(np, count)

        cur = next
        next = Counter()

    return sum(cur.values())


def main():
    run_on_inputs(run, {1: 40})


if __name__ == "__main__":
    main()
