#!/usr/bin/env python3

from lib import *


def run(inp: Input):
    grid = {p: c for p, c in inp.as_pos}

    res = 0
    for p, c in grid.items():
        if c != "@": continue
        cnt = 0
        for d in V.directions(diagonal=True):
            np = p + d
            if grid.get(np) == "@": cnt += 1
        if cnt < 4: res += 1

    return res


def main():
    run_on_inputs(run, {1: 13})


if __name__ == "__main__":
    main()
