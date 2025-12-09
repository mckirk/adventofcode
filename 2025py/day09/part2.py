#!/usr/bin/env python3

from lib import *
import itertools
import shapely

def run(inp: Input):
    res = 0
    tiles = [V(*d) for d in inp.parsed]

    ring = shapely.LinearRing((t.x, t.y) for t in tiles)
    poly = shapely.Polygon(ring)

    for t1, t2 in itertools.combinations(tiles, 2):
        min_x, min_y = min(t1.x, t2.x), min(t1.y, t2.y)
        max_x, max_y = max(t1.x, t2.x), max(t1.y, t2.y)

        rect = shapely.Polygon([
            (min_x, min_y),
            (max_x, min_y),
            (max_x, max_y),
            (min_x, max_y),
        ])

        if not poly.covers(rect):
            continue

        d = abs(t1-t2)+V(1,1)
        res = max(res, d.x*d.y)

    return res


def main():
    run_on_inputs(run, {1: 24})


if __name__ == "__main__":
    main()
