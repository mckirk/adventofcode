#!/usr/bin/env python3

from lib import *


def run(inp: Input):
    grid = {p: c for p, c in inp.as_pos}

    grid_inv = defaultdict(list)
    for p, c in grid.items():
        grid_inv[c].append(p)

    start = grid_inv["S"][0]
    splitters = set(grid_inv["^"])

    res = 0
    cur = set([start])
    next = set()

    while cur:
        def add_next(p: V):
            if p in grid:
                next.add(p)
        
        for p in cur:
            np = p + V(0, 1)
            if np in splitters:
                res += 1
                add_next(np + (V(1,0)))
                add_next(np + (V(-1,0)))
            else:
                add_next(np)

        cur = next
        next = set()

    return res


def main():
    run_on_inputs(run, {1: 21})


if __name__ == "__main__":
    main()
