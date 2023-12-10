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


def directions():
    for xd in [-1, 0, 1]:
        for yd in [-1, 0, 1]:
            if xd == yd == 0:
                continue
            yield (xd, yd)


def adj(p, limits):
    for d in directions():
        x2, y2 = add(p, d)
        if x2 < 0 or x2 >= limits[0]:
            continue
        if y2 < 0 or y2 >= limits[1]:
            continue
        yield ((x2, y2), d)


def line(pos, dir, limits):
    x, y = pos
    xd, yd = dir
    i = 1
    while True:
        x2 = x+xd*i
        y2 = y+yd*i
        if x2 < 0 or x2 > limits[0]:
            break
        if y2 < 0 or y2 > limits[1]:
            break
        yield (x2, y2)
        i += 1


def inv(d):
    xd, yd = d
    return (-xd, -yd)


def add(p, d):
    return tuple(v1+v2 for v1, v2 in zip(p, d))


def sub(p1, p2):
    return tuple(v1-v2 for v1, v2 in zip(p1, p2))


def dist(p1, p2):
    return sum(abs(v1 - v2) for (v1, v2) in zip(p1, p2))