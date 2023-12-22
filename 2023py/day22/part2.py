#!/usr/bin/env python3
from collections import defaultdict, Counter
from dataclasses import field
from functools import reduce
from pathlib import Path
from pprint import pprint
from aocparser import parse
from intervaltree import Interval, IntervalTree

from lib import *

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

spec = "[[{i}|,]~[{i}|,]|\n]"


@dataclass(eq=False)
class Cube:
    start: V3
    end: V3

    landed_at_z: int | None = field(default=None)

    would_fall_if: set["Cube"] = field(default_factory=set)

    def __post_init__(self):
        assert self.start.x <= self.end.x
        assert self.start.y <= self.end.y
        assert self.start.z <= self.end.z

    @property
    def x_interval(self):
        return Interval(self.start.x, self.end.x+1, self)
    
    @property
    def y_interval(self):
        return Interval(self.start.y, self.end.y+1, self)
    
    @property
    def landed_z_end(self):
        return self.landed_at_z + self.end.z - self.start.z
    
    def __hash__(self):
        return hash(id(self))
    
    def __eq__(self, other):
        return self is other
    
    def land(self, grid: "Grid"):
        if self.landed_at_z is not None:
            return
        
        overlap = grid.overlap(self)

        highest_z = 0
        resting_on = []

        for o in overlap:
            if o.start.z > self.end.z or o is self:
                continue

            o.land(grid)

            if o.landed_z_end > highest_z:
                highest_z = o.landed_z_end
                resting_on = [o]
            elif o.landed_z_end == highest_z:
                resting_on.append(o)
        
        self.landed_at_z = highest_z + 1

        if resting_on:
            would_fall_if_sets = [o.would_fall_if | {o} for o in resting_on]
            self.would_fall_if = reduce(lambda a, b: a & b, would_fall_if_sets)
    

class Grid:
    def __init__(self):
        self.x = IntervalTree()
        self.y = IntervalTree()

    def add(self, cube: Cube):
        self.x.add(cube.x_interval)
        self.y.add(cube.y_interval)

    def overlap(self, cube: Cube) -> set[Cube]:
        overlap_x = self.x.overlap(cube.x_interval)
        overlap_y = self.y.overlap(cube.y_interval)

        overlap = {o.data for o in overlap_x} & {o.data for o in overlap_y}
        return overlap


def main():
    bricks = [Cube(V3(*s), V3(*e)) for s, e in parse(spec, input)]

    grid = Grid()

    for b in bricks:
        grid.add(b)

    for b in bricks:
        b.land(grid)

    res = 0
    for b in bricks:
        would_fall = sum(1 for b2 in bricks if b in b2.would_fall_if and b2 is not b)
        res += would_fall

    print(res)
if __name__ == "__main__":
    main()
