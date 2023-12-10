#!/usr/bin/env python3
from abc import ABC, abstractmethod
from collections import defaultdict, Counter
from dataclasses import dataclass
from pathlib import Path
from pprint import pprint
from typing import Iterable
from aocparser import parse
import colored

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


B = {
    "|": [V(0, 1), V(0, -1)],
    "-": [V(1, 0), V(-1, 0)],
    "L": [V(0, -1), V(1, 0)],
    "J": [V(0, -1), V(-1, 0)],
    "7": [V(0, 1), V(-1, 0)],
    "F": [V(0, 1), V(1, 0)],
}


def find_loop(start, tiles):
    seen = {start: []}
    pos = [(start, [])]
    loop = None

    while pos:
        # print("#"*10)
        # pprint_board(lines, {p: "green" for p, _ in pos})
        # print("#"*10)
        # print()

        next_pos = []
        while pos:
            cur, cur_path = pos.pop()
            cur_t = tiles[cur]

            if cur_t == "S":
                ds = V.directions()
            else:
                ds = B[cur_t]

            for d in ds:
                o = cur + d
                next_path = cur_path + [o]
                seen_path = seen.get(o)
                if seen_path is not None:
                    if len(seen_path) == len(next_path):
                        loop = [start] + cur_path + list(reversed(seen_path))
                        break
                    if len(seen_path) <= len(next_path):
                        continue

                t = tiles.get(o)
                if not t or not t in B:
                    continue

                if -d in B[t]:
                    next_pos.append((o, next_path))
                    seen[o] = next_path

            if loop:
                break

        pos = next_pos
        if loop:
            break

    print(len(loop))

    return loop


def blow_up(lines, loop: list[V]):
    new_tiles = dict()
    new_loop = list()
    for y, l in enumerate(lines):
        for x, c in enumerate(l):
            new_tiles[2*V(x, y)] = c

    cur = None

    new_loop = []
    for p in loop + [loop[0]]:
        np = 2*p

        if cur is not None:
            d = np - cur

            if d.x != 0:
                assert d.y == 0
                i = np - (d // 2)
                new_tiles[i] = "-"
                new_loop.append(i)
            elif d.y != 0:
                assert d.x == 0
                i = np - (d // 2)
                new_tiles[i] = "|"
                new_loop.append(i)
            else:
                assert False

        new_loop.append(np)
        cur = np

    return new_tiles, new_loop


def main():
    list_board = ListBoard(lines)
    dict_board = DictBoard(list_board.to_dict(), " ")
    start = dict_board.find("S")

    loop = find_loop(start, dict_board.board)
    # list_board.pprint({p: "red" for p in loop})

    ntiles, nloop = blow_up(lines, loop)
    nboard = DictBoard(ntiles, ".")
    # nboard.pprint({p: "red" for p in nloop})
    nlimits = (len(lines[0]) * 2, len(lines) * 2)
    nloops = set(nloop)

    outside: set[V] = set()
    for x in range(nlimits[0]):
        outside.add(V(x, -1))
        outside.add(V(x, nlimits[1]))
    for y in range(nlimits[1]):
        outside.add(V(-1, y))
        outside.add(V(nlimits[0], y))

    cur_outside = set(outside)

    while cur_outside:
        new_outside = set()

        for p in cur_outside:
            for a, d in p.adj(nlimits):
                if not a in outside and not a in nloops:
                    new_outside.add(a)
                    outside.add(a)
        cur_outside = new_outside

        # nboard.pprint({p: "red" for p in nloop} | {p: "green" for p in outside})

    inside = set(nboard.board.keys()) - outside - nloops

    print(len(inside))


if __name__ == "__main__":
    main()
