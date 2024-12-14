#!/usr/bin/env python3
from lib import *

DIMS = (101, 103)
out = open("out.txt", "w")


def run(inp: Input):
    robots = []
    for px, py, vx, vy in inp.parsed:
        p = V(px, py)
        v = V(vx, vy)
        robots.append([p, v])

    for i in range(10000):
        print(f"### {i} ###", file=out)
        has = {p for p, _ in robots}
        for y in range(DIMS[1]):
            for x in range(DIMS[0]):
                print("#" if V(x, y) in has else ".", end="", file=out)
            print(file=out)
        print(file=out)

        for r in robots:
            r[0] = (r[0] + r[1]).tiled(DIMS)
    return None


def main():
    run_on_inputs(run)


if __name__ == "__main__":
    main()
