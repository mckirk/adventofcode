from pathlib import Path

import aocparser

script_dir = Path(__file__).parent
spec_file = script_dir / "spec.aocp"


class Input:
    def __init__(self, path: Path):
        self.path = path
        if not path.is_file():
            self.content = None
            return

        self.content = path.read_text().strip()
        self.lines = self.content.splitlines()
        self.blocks = self.content.split(r"\n\n")

        if spec_file.is_file() and (spec := spec_file.read_text()):
            self.parsed = aocparser.parse(spec, self.content)
        else:
            self.parsed = None

    @property
    def exists(self):
        return bool(self.content)
    
    @property
    def limits(self):
        return (len(self.lines[0]), len(self.lines))
    
    @property
    def as_pos(self):
        for y, l in enumerate(self.lines):
            for x, c in enumerate(l):
                yield (x, y), c


problem_input = Input(script_dir / "input.txt")
sample1_input = Input(script_dir / "sample1.txt")
sample2_input = Input(script_dir / "sample2.txt")


def run_on_inputs(sample1_expected, sample2_expected, run):
    for i, (inp, exp) in enumerate(
        [(sample1_input, sample1_expected), (sample2_input, sample2_expected)], start=1
    ):
        if inp.exists:
            res = run(inp)
            print(f"Sample {i}: {res}")
            if exp is not None and res != exp:
                print(
                    f"Did not match expected result for sample {i} ({exp}), aborting."
                )
                return

    res = run(problem_input)
    print(f"Result: {res}")


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

    def tiled(self, limits):
        return V(self.x % limits[0], self.y % limits[1])
    
    def tile(self, limits):
        return V(self.x // limits[0], self.y // limits[1])
    
    def elem_mul(self, other):
        return V(self.x * other.x, self.y * other.y)


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