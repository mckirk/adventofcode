#!/usr/bin/env python3
from abc import ABC, abstractmethod
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from typing import Iterable
from aocparser import parse
import colored

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()


def directions():
    for xd in [-1, 0, 1]:
        for yd in [-1, 0, 1]:
            if xd == yd == 0:
                continue
            yield (xd, yd)


def inv(d):
    xd, yd = d
    return (-xd, -yd)


def add(p, d):
    return tuple(v1 + v2 for v1, v2 in zip(p, d))


def sub(p1, p2):
    return tuple(v1 - v2 for v1, v2 in zip(p1, p2))


def adj(p, limits):
    for d in directions():
        x2, y2 = add(p, d)
        if x2 < 0 or x2 >= limits[0]:
            continue
        if y2 < 0 or y2 >= limits[1]:
            continue
        yield ((x2, y2), d)


def dist(p1, p2):
    return sum(abs(v1 - v2) for (v1, v2) in zip(p1, p2))


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
                tiles[(x, y)] = c

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
                l += self.board.get((x, y), self.fill)
            yield l

    def to_list(self):
        return list(self.by_lines())

    def find(self, e):
        for p, c in self.board.items():
            if c == e:
                return p

        return None


B = {
    "|": [(0, 1), (0, -1)],
    "-": [(1, 0), (-1, 0)],
    "L": [(0, -1), (1, 0)],
    "J": [(0, -1), (-1, 0)],
    "7": [(0, 1), (-1, 0)],
    "F": [(0, 1), (1, 0)],
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
                ds = directions()
            else:
                ds = B[cur_t]

            for d in ds:
                o = add(cur, d)
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

                if inv(d) in B[t]:
                    next_pos.append((o, next_path))
                    seen[o] = next_path

            if loop:
                break

        pos = next_pos
        if loop:
            break

    print(len(loop))

    return loop


def blow_up(lines, loop):
    new_tiles = dict()
    new_loop = list()
    for y, l in enumerate(lines):
        for x, c in enumerate(l):
            new_tiles[(2 * x, 2 * y)] = c

    cur = None

    new_loop = []
    for p in loop + [loop[0]]:
        px, py = p
        npx, npy = 2 * px, 2 * py
        np = (npx, npy)

        if cur is not None:
            dx, dy = sub(np, cur)

            if dx != 0:
                assert dy == 0
                i = (npx - (dx // 2), npy)
                new_tiles[i] = "-"
                new_loop.append(i)
            elif dy != 0:
                assert dx == 0
                i = (npx, npy - (dy // 2))
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

    outside = set()
    for x in range(nlimits[0]):
        outside.add((x, -1))
        outside.add((x, nlimits[1]))
    for y in range(nlimits[1]):
        outside.add((-1, y))
        outside.add((nlimits[0], y))

    cur_outside = set(outside)

    while cur_outside:
        new_outside = set()

        for p in cur_outside:
            for a, d in adj(p, nlimits):
                if not a in outside and not a in nloops:
                    new_outside.add(a)
                    outside.add(a)
        cur_outside = new_outside

        # nboard.pprint({p: "red" for p in nloop} | {p: "green" for p in outside})

    inside = set(nboard.board.keys()) - outside - nloops

    print(len(inside))


if __name__ == "__main__":
    main()
