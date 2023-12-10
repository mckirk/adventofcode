#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from aocparser import parse
import colored

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
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
    return tuple(v1+v2 for v1, v2 in zip(p, d))


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


def pprint_board(board: list[list], colors: dict[tuple, str]):
    ls = []
    for y, l in enumerate(lines):
        cur_l = ""
        for x, c in enumerate(l):
            color = colors.get((x, y))
            if color:
                cur_l += f"{colored.fore(color)}{c}{colored.Style.reset}"
            else:
                cur_l += c
        ls.append(cur_l)

    print("\n".join(ls))


B = {
    "|": [(0, 1), (0, -1)],
    "-": [(1, 0), (-1, 0)],
    "L": [(0, -1), (1, 0)],
    "J": [(0, -1), (-1, 0)],
    "7": [(0, 1), (-1, 0)],
    "F": [(0, 1), (1, 0)],
}

def main():
    tiles = dict()
    tiles_arr = []
    start = None
    for y, l in enumerate(lines):
        tiles_arr.append([None]*len(l))
        for x, c in enumerate(l):
            tiles[(x, y)] = c
            tiles_arr[y][x] = c
            if c == "S":
                start = (x, y)

    limits = (len(lines[0]), len(lines))

    seen = {start: 0}
    pos = [(start, 0)]
    max_dist, max_p = 0, None

    while pos:
        # print("#"*10)
        # pprint_board(lines, {p: "green" for p, _ in pos})
        # print("#"*10)
        # print()

        next_pos = []
        while pos:
            cur, cur_dist = pos.pop()
            next_dist = cur_dist+1
            cur_t = tiles[cur]

            if cur_t == "S":
                ds = directions()
            else:
                ds = B[cur_t]

            for d in ds:
                o = add(cur, d)
                seen_dist = seen.get(o)
                if seen_dist is not None and seen_dist <= next_dist:
                    continue

                t = tiles.get(o)
                if not t or not t in B:
                    continue

                if inv(d) in B[t]:
                    next_pos.append((o, next_dist))
                    seen[o] = next_dist

                    if next_dist > max_dist:
                        max_dist = next_dist
                        max_p = o

        pos = next_pos
    
    print(max_dist, max_p)

    
if __name__ == "__main__":
    main()
