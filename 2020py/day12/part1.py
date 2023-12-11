#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from aocparser import parse

from dataclasses import dataclass
from enum import Enum


@dataclass(frozen=True, unsafe_hash=True)
class V:
    x: int
    y: int

    def __add__(self, other):
        return V(self.x + other.x, self.y + other.y)
    
    def __sub__(self, other):
        return V(self.x - other.x, self.y - other.y)
    
    def __mul__(self, other):
        return V(self.x * other, self.y * other)
    
    def __rmul__(self, other) -> "V":
        return self * other
    
    def __floordiv__(self, other):
        return V(self.x // other, self.y // other)
    
    def __neg__(self):
        return V(-self.x, -self.y)
    
    def __abs__(self):
        return V(abs(self.x), abs(self.y))
    
    def __iter__(self):
        yield self.x
        yield self.y
    
    def within(self, limits):
        return 0 <= self.x < limits[0] and 0 <= self.y < limits[1]
    
    def dist(self, other):
        return sum(abs(self - other))

    @classmethod
    def directions(cls):
        for xd in [-1, 0, 1]:
            for yd in [-1, 0, 1]:
                if xd == yd == 0:
                    continue
                yield cls(xd, yd)

    def adj(self, limits):
        for d in self.directions():
            p2 = self + d
            if p2.within(limits):
                yield p2, d


class Dir(Enum):
    N = V(0, -1)
    NE = V(1, -1)
    E = V(1, 0)
    SE = V(1, 1)
    S = V(0, 1)
    SW = V(-1, 1)
    W = V(-1, 0)
    NW = V(-1, -1)

    def turn(self, degrees_right):
        transl = {
            self.N: 0,
            self.NE: 1,
            self.E: 2,
            self.SE: 3,
            self.S: 4,
            self.SW: 5,
            self.W: 6,
            self.NW: 7
        }
        assert (degrees_right // 45) * 45 == degrees_right
        cur = transl[self]
        new = (cur + degrees_right // 45) % 8
        return list(transl.keys())[new]


input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    ins = []
    for l in lines:
        ins.append((l[0], int(l[1:])))

    cur_d = Dir.E
    cur_pos = V(0, 0)
    for a, v in ins:
        if hasattr(Dir, a):
            cur_pos = cur_pos + Dir[a].value*v
        if a == "F":
            cur_pos += cur_d.value*v
        if a == "R":
            cur_d = cur_d.turn(v)
        if a == "L":
            cur_d = cur_d.turn(-v)
        # print(f"{a}{v} -> {cur_pos} ({cur_d})")

    print(V(0, 0).dist(cur_pos))

    
if __name__ == "__main__":
    main()
