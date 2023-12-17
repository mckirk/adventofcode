from dataclasses import dataclass
from enum import Enum
from typing import Any, Generic, Optional, TypeVar, Callable
import heapq
from typing import Generic, TypeVar

T = TypeVar("T")


class PriorityQueue(Generic[T]):
    def __init__(self):
        self.elements = []

    def empty(self):
        return len(self.elements) == 0

    def put(self, item: T):
        heapq.heappush(self.elements, item)

    def get(self) -> T:
        return heapq.heappop(self.elements)


class Base(Generic[T]):
    def __init__(
        self, elem: Optional[T] = None, key: Optional[Callable[[T], Any]] = None
    ):
        self.elem = elem
        self.key = key if key is not None else lambda x: x

    def empty(self):
        return self.elem is None

    def get(self) -> T:
        assert self.elem is not None
        return self.elem


class Min(Generic[T], Base[T]):
    def update(self, elem: T):
        if self.elem is None or self.key(elem) < self.key(self.elem):
            self.elem = elem
            return True
        return False


class Max(Generic[T], Base[T]):
    def update(self, elem: T):
        if self.elem is None or self.key(elem) > self.key(self.elem):
            self.elem = elem
            return True
        return False


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
