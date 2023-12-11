#!/usr/bin/env python3
from abc import ABC, abstractmethod
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from typing import Iterable
from aocparser import parse
import colored
from dataclasses import dataclass

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()


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
                tiles[V(x, y)] = c

        return tiles


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
                l += self.board.get(V(x, y), self.fill)
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


def main():
    empties_x = [x for x, l in enumerate(transpose(lines)) if all(c == "." for c in l)]
    empties_y = [y for y, l in enumerate(lines) if all(c == "." for c in l)]

    lboard = ListBoard(lines)
    dboard = DictBoard(lboard.to_dict(), fill="o")

    dboard.pprint({})

    universes = list(dboard.find_all("#"))
    print(len(universes))

    res = 0
    for u in universes:
        for u2 in universes:
            if u == u2:
                continue
            dist_naive = u.dist(u2)
            dist = dist_naive
            for x in empties_x:
                if u.x < x < u2.x or u2.x < x < u.x:
                    dist += 999999
            for y in empties_y:
                if u.y < y < u2.y or u2.y < y < u.y:
                    dist += 999999
            res += dist

    print(res // 2)


if __name__ == "__main__":
    main()
