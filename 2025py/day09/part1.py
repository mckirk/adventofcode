#!/usr/bin/env python3

from lib import *
import itertools

def run(inp: Input):
    res = 0
    tiles = [V(*d) for d in inp.parsed]
    for t1, t2 in itertools.combinations(tiles, 2):
        d = abs(t1-t2)+V(1,1)
        res = max(res, d.x*d.y)
    return res


def main():
    run_on_inputs(run, {1: 50})


if __name__ == "__main__":
    main()
