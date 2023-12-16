#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
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
    
    def line(self, dir, limits):
        p2 = self
        while True:
            p2 += dir
            if not p2.within(limits):
                break
            yield p2


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
            self.NW: 7,
        }
        assert (degrees_right // 45) * 45 == degrees_right
        cur = transl[self]
        new = (cur + degrees_right // 45) % 8
        return list(transl.keys())[new]


def directions():
    for xd in [-1, 0, 1]:
        for yd in [-1, 0, 1]:
            if xd == yd == 0:
                continue
            yield V(xd, yd)


def get_positions(lines, look_for):
    pos = set()
    for y, l in enumerate(lines):
        for x, c in enumerate(l):
            if c == look_for:
                pos.add(V(x, y))

    return pos

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

def main():
    board = dict()
    for y, l in enumerate(lines):
        for x, c in enumerate(l):
            board[V(x, y)] = c

    potentials = set()
    for x in range(len(lines[0])):
        potentials.add((V(x, -1), V(0, 1)))
        potentials.add((V(x, len(lines)), V(0, -1)))
    for y in range(len(lines)):
        potentials.add((V(-1, y), V(1, 0)))
        potentials.add((V(len(lines), y), V(-1, 0)))

    max_energized = 0

    for p in potentials:
        dirs_per_pos = defaultdict(set)
        energized = set()

        to_do = {p}

        while to_do:
            cur = to_do.pop()
            pos, dir = cur
            dirs_per_pos[pos].add(dir)
            energized.add(pos)

            new_pos = pos + dir

            def maybe_add(new_dir):
                if new_dir not in dirs_per_pos[new_pos]:
                    to_do.add((new_pos, new_dir))
                    # print(f"Adding {new_pos} {new_dir}")

            nc = board.get(new_pos)
            if nc is None:
                continue

            # print(f"Doing {pos} {dir}, nc {nc}")

            if nc == "/":
                if dir == V(1, 0):
                    maybe_add(V(0, -1))
                elif dir == V(0, 1):
                    maybe_add(V(-1, 0))
                elif dir == V(-1, 0):
                    maybe_add(V(0, 1))
                elif dir == V(0, -1):
                    maybe_add(V(1, 0))
                else:
                    raise ValueError(dir)
            elif nc == "\\":
                if dir == V(1, 0):
                    maybe_add(V(0, 1))
                elif dir == V(0, 1):
                    maybe_add(V(1, 0))
                elif dir == V(-1, 0):
                    maybe_add(V(0, -1))
                elif dir == V(0, -1):
                    maybe_add(V(-1, 0))
                else:
                    raise ValueError(dir)
            elif nc == "-":
                if dir.y != 0:
                    maybe_add(V(1, 0))
                    maybe_add(V(-1, 0))
                else:
                    maybe_add(dir)
            elif nc == "|":
                if dir.x != 0:
                    maybe_add(V(0, 1))
                    maybe_add(V(0, -1))
                else:
                    maybe_add(dir)
            elif nc == ".":
                maybe_add(dir)

        max_energized = max(max_energized, len(energized) - 1)

    print(max_energized)


    
if __name__ == "__main__":
    main()
