#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from aocparser import parse
from abc import ABC, abstractmethod
from typing import Iterable
import colored
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


input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

def main():
    board = ListBoard(lines)
    dict_board = DictBoard(board.to_dict(), fill=".")

    while True:
        new_tiles = dict()
        limits = board.limits

        for y in range(limits[1]):
            for x in range(limits[0]):
                p = (x, y)
                if not p in dict_board.board:
                    continue
                if dict_board.board[p] == "#":
                    new_tiles[p] = "#"
                if dict_board.board[p] == "O":
                    v = V(*p)
                    new_v = v+V(0, -1)
                    if new_v.y < 0 or (new_v.x, new_v.y) in new_tiles:
                        new_tiles[p] = "O"
                    else:
                        new_tiles[(new_v.x, new_v.y)] = "O"

        if new_tiles == dict_board.board:
            break
        dict_board.board = new_tiles

    dict_board.pprint({})

    res = 0
    for p in dict_board.find_all("O"):
        res += limits[1]-p[1]
    print(res)

    
if __name__ == "__main__":
    main()
