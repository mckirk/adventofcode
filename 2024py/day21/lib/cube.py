from dataclasses import dataclass, field
from functools import reduce
from typing import Optional
from intervaltree import Interval, IntervalTree


@dataclass
class V3:
    x: int
    y: int
    z: int


@dataclass(eq=False)
class Cube:
    start: V3
    end: V3

    landed_at_z: Optional[int] = field(default=None)

    would_fall_if: set["Cube"] = field(default_factory=set)

    def __post_init__(self):
        assert self.start.x <= self.end.x
        assert self.start.y <= self.end.y
        assert self.start.z <= self.end.z

    @property
    def x_interval(self):
        return Interval(self.start.x, self.end.x + 1, self)

    @property
    def y_interval(self):
        return Interval(self.start.y, self.end.y + 1, self)

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
