#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from aocparser import parse
from intervaltree import Interval, IntervalTree

from lib import *

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

spec = "[[{i}|,]~[{i}|,]|\n]"


@dataclass
class Cube:
    start: V3
    end: V3

    is_free: bool = True

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
    
    def __hash__(self):
        return id(self)


def main():
    bricks = [Cube(V3(*s), V3(*e)) for s, e in parse(spec, input)]
    bricks.sort(key=lambda c: min(c.start.z, c.end.z))

    landed_x = IntervalTree()
    landed_y = IntervalTree()

    for b in bricks:
        overlap_x = landed_x.overlap(b.x_interval)
        overlap_y = landed_y.overlap(b.y_interval)

        overlap = {o.data for o in overlap_x} & {o.data for o in overlap_y}

        highest_z = 0
        resting_on = []

        if overlap:
            for o in overlap:
                if o.end.z > highest_z:
                    highest_z = o.end.z
                    resting_on = [o]
                elif o.end.z == highest_z:
                    resting_on.append(o)
            new_z = highest_z + 1

            if len(resting_on) == 1:
                resting_on[0].is_free = False
        else:
            new_z = 1

        delta_z = b.start.z - new_z

        b.start.z -= delta_z
        b.end.z -= delta_z

        landed_x.add(b.x_interval)
        landed_y.add(b.y_interval)

    print(sum(b.is_free for b in bricks))

if __name__ == "__main__":
    main()
