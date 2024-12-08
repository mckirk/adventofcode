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


from abc import ABC, abstractmethod
from typing import Iterable
import colored


def get_limits(lines):
    return (len(lines[0]), len(lines))


def get_pos(lines, look_for):
    pos = set()
    for y, l in enumerate(lines):
        for x, c in enumerate(l):
            if c == look_for:
                pos.add((x, y))

    return pos


class Board(ABC):
    def pprint(self, colors: dict[tuple, str]):
        ls = []
        for y, l in enumerate(self.by_lines()):
            cur_l = ""
            for x, c in enumerate(l):
                color = colors.get((x, y))
                if color:
                    cur_l += f"{colored.fore(color)}{c}{colored.Style.reset}"
                else:
                    cur_l += c
            ls.append(cur_l)

        print("\n".join(ls))

    @abstractmethod
    def by_lines(self) -> Iterable:
        ...


def num_diff(s1, s2):
    return sum(1 for x, y in zip(s1, s2) if x != y)


class ListBoard(Board):
    def __init__(self, board: list[str]):
        self.board = board
        self.limits = (len(board[0]), len(board))

    def by_lines(self):
        return self.board

    def to_dict(self):
        tiles = dict()
        for y, l in enumerate(self.board):
            for x, c in enumerate(l):
                tiles[(x, y)] = c

        return tiles
    
    def find_reflection_y(self, num_defects=0):
        for i, _ in enumerate(self.board):
            parts = self.board[:i][::-1], self.board[i:]
            min_len = min(len(p) for p in parts)
            if not min_len:
                continue
            parts = [p[:min_len] for p in parts]
            if num_defects:
                total_num_diff = sum(num_diff(*p) for p in zip(*parts))
                if total_num_diff == num_defects:
                    return i
            else:
                if parts[0] == parts[1]:
                    return i
        return None


class DictBoard(Board):
    def __init__(self, board: dict[tuple, str], fill: str):
        self.board = board
        self.fill = fill
        self.limits = (max(x + 1 for x, y in board), max(y + 1 for x, y in board))

    def by_lines(self):
        xl, yl = self.limits
        for y in range(yl):
            l = ""
            for x in range(xl):
                l += self.board.get((x, y), self.fill)
            yield l

    def to_list(self):
        return list(self.by_lines())

    def find(self, e):
        for p, c in self.board.items():
            if c == e:
                return p

        return None

    def find_all(self, e):
        for p, c in self.board.items():
            if c == e:
                yield p


def transpose(lines):
    return ["".join(l) for l in zip(*lines)]


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