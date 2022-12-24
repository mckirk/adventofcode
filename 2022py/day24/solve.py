#!/usr/bin/env python3

from collections import defaultdict
import itertools
from os import path

blocked = set()
blizzards = set()

dir_map = {">": (1, 0), "<": (-1, 0), "^": (0, -1), "v": (0, 1)}
inv_map = {d: c for c, d in dir_map.items()}

with open(path.join(path.dirname(__file__), "input.txt")) as f:
    ls = f.readlines()
    for y, l in enumerate(ls):
        for x, c in enumerate(l.strip()):
            p = (x, y)
            if c == "#":
                blocked.add(p)
            elif c != ".":
                blizzards.add((p, dir_map[c]))

maxx = max(x for x, y in blocked)
maxy = max(y for x, y in blocked)


def add(p1, p2):
    return tuple(a+b for a, b in zip(p1, p2))


dirs = [(0, 1), (0, -1), (1, 0), (-1, 0)]


def draw(blizzards, es):
    ls = []
    for y in range(0, maxy+1):
        l = []
        for x in range(0, maxx+1):
            if (x, y) in blocked:
                l += ["#"]
            elif (x, y) in es:
                l += ["E"]
            else:
                l += ["."]
        ls.append(l)

    blizzarded = defaultdict(list)

    for bp, bd in blizzards:
        blizzarded[bp].append(inv_map[bd])

    for (bx, by), bds in blizzarded.items():
        assert ls[by][bx] == "."
        if len(bds) == 1:
            ls[by][bx] = bds[0]
        else:
            ls[by][bx] = str(len(bds))

    for l in ls:
        print("".join(l))
    print("-"*20)


def sim(start, target):
    global blizzards

    es = set()
    es.add(start)

    for i in itertools.count():
        # draw()
        if target in es:
            break

        next_blizzards = set()
        blizzarded = set()
        for bp, bd in blizzards:
            (nx, ny) = add(bp, bd)
            if nx == 0:
                nx = maxx - 1
            if nx == maxx:
                nx = 1
            if ny == 0:
                ny = maxy - 1
            if ny == maxy:
                ny = 1

            next_blizzards.add(((nx, ny), bd))
            blizzarded.add((nx, ny))

        next_es = set()
        for e in es:
            if e not in blizzarded:
                next_es.add(e)
            for d in dirs:
                p = add(e, d)
                if p not in blizzarded and p not in blocked and p[1] >= 0 and p[1] <= maxy:
                    next_es.add(p)

        blizzards = next_blizzards
        es = next_es

    return i


part1 = sim((1, 0), (maxx-1, maxy))
print(part1)

part2 = part1
part2 += sim((maxx-1, maxy), (1, 0))
part2 += sim((1, 0), (maxx-1, maxy))
print(part2)
