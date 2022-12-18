#!/usr/bin/env python3

from os import path
import re
import sys

sys.setrecursionlimit(1000000000)


def ints(string):
    return list(map(int, re.findall(r"-?[0-9]+", string)))


droplets = set()
with open(path.join(path.dirname(__file__), "input.txt")) as f:
    for l in f.readlines():
        droplets.add(tuple(ints(l)))


mins = [None]*3
maxs = [None]*3
lens = [None]*3
n = 1

for i in range(3):
    mins[i] = min(t[i] for t in droplets)
    maxs[i] = max(t[i] for t in droplets)
    lens[i] = maxs[i] - mins[i]

    n *= lens[i]

sides1 = 0
sides2 = 0
dirs = [(-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1)]


def searchOut(p, seen):
    x, y, z = p

    seen.add(p)

    for (xd, yd, zd) in dirs:
        o = xo, yo, zo = x+xd, y+yd, z+zd

        if o in droplets:
            continue
        elif xo > maxs[0] or yo > maxs[1] or zo > maxs[2] or \
                xo < mins[0] or yo < mins[1] or zo < mins[2]:
            return True
        elif o not in seen:
            if searchOut(o, seen):
                return True

    return False


for x, y, z in droplets:

    for (xd, yd, zd) in dirs:
        xo, yo, zo = x+xd, y+yd, z+zd

        if (xo, yo, zo) not in droplets:
            sides1 += 1

            if searchOut((xo, yo, zo), set()):
                sides2 += 1

print(sides1)
print(sides2)
