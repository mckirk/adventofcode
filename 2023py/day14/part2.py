#!/usr/bin/env python3
from collections import defaultdict, Counter
from functools import cache
from pathlib import Path
from pprint import pprint
from abc import ABC, abstractmethod
from typing import Iterable
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

    def turn(self, turns_right):
        turns_right %= 4

        if turns_right == 0:
            return self
        if turns_right == 1:
            return V(-self.y, self.x)
        if turns_right == 2:
            return -self
        if turns_right == 3:
            return V(self.y, -self.x)
        raise ValueError(turns_right)
    
    def dot(self, other):
        return self.x * other.x + self.y * other.y
    
    @property
    def tuple(self):
        return (self.x, self.y)


def get_limits(lines):
    return (len(lines[0]), len(lines))


def get_pos(lines, look_for):
    pos = set()
    for y, l in enumerate(lines):
        for x, c in enumerate(l):
            if c == look_for:
                pos.add((x, y))

    return pos


class Dir(Enum):
    N = V(0, -1)
    NE = V(1, -1)
    E = V(1, 0)
    SE = V(1, 1)
    S = V(0, 1)
    SW = V(-1, 1)
    W = V(-1, 0)
    NW = V(-1, -1)


def get_positions(lines, look_for):
    pos = set()
    for y, l in enumerate(lines):
        for x, c in enumerate(l):
            if c == look_for:
                pos.add(V(x, y))

    return pos


def transpose(lines):
    return ["".join(l) for l in zip(*lines)]


input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

def main():
    limits = get_limits(lines)
    movable, immovable = frozenset(get_positions(lines, "O")), frozenset(get_positions(lines, "#"))
    seen = dict()

    LIMIT = 1000000000
    for i in range(LIMIT):
        h = movable
        if h in seen:
            loop_size = i - seen[h]
            if (LIMIT-i)%loop_size == 0:
                print(f"Found loop at {i} with size {loop_size}!")
                break
        else:
            seen[h] = i
        
        for d in [Dir.N, Dir.W, Dir.S, Dir.E]:
            while True:
                new_movable = set()
                
                pos_candidates = sorted(movable, key=lambda p: -p.dot(d.value))
                for p in pos_candidates:
                    new_v = p+d.value
                    if not new_v.within(limits) or new_v in new_movable or new_v in immovable:
                        new_movable.add(p)
                    else:
                        new_movable.add(new_v)
                
                frozen = frozenset(new_movable)
                if frozen == movable:
                    break
                movable = frozen

        # dict_board.pprint({})
        # print("#"*20)

    res = 0
    for p in movable:
        res += limits[1]-p.y
    print(i, res)
    
if __name__ == "__main__":
    main()
