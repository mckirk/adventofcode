#!/usr/bin/env python3
from functools import reduce
import operator
from lib import *


def run(inp: Input, dims):
    half = (dims[0] // 2, dims[1] // 2)
    res = [0, 0, 0, 0]
    for px, py, vx, vy in inp.parsed:
        p = V(px, py)
        v = V(vx, vy)
        for _ in range(100):
            p += v
        p = p.tiled(dims)
        if p.x == half[0]:
            continue
        if p.y == half[1]:
            continue
        q = p.tile(tuple(d + 1 for d in half))
        res[q.x + q.y * 2] += 1
    return reduce(operator.mul, res)


def main():
    run_on_inputs(run, {1: 12}, dims={1: (11, 7), None: (101, 103)})


if __name__ == "__main__":
    main()
