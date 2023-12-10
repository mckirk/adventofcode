from enum import Enum


class Dir(Enum):
    N = (0, -1)
    NE = (1, -1)
    E = (1, 0)
    SE = (1, 1)
    S = (0, 1)
    SW = (-1, 1)
    W = (-1, 0)
    NW = (-1, -1)


def directions():
    for xd in [-1, 0, 1]:
        for yd in [-1, 0, 1]:
            if xd == yd == 0:
                continue
            yield (xd, yd)


def adj(p, limits):
    for d in directions():
        x2, y2 = add(p, d)
        if x2 < 0 or x2 >= limits[0]:
            continue
        if y2 < 0 or y2 >= limits[1]:
            continue
        yield ((x2, y2), d)


def line(pos, dir, limits):
    x, y = pos
    xd, yd = dir
    i = 1
    while True:
        x2 = x+xd*i
        y2 = y+yd*i
        if x2 < 0 or x2 > limits[0]:
            break
        if y2 < 0 or y2 > limits[1]:
            break
        yield (x2, y2)
        i += 1


def inv(d):
    xd, yd = d
    return (-xd, -yd)


def add(p, d):
    return tuple(v1+v2 for v1, v2 in zip(p, d))


def sub(p1, p2):
    return tuple(v1-v2 for v1, v2 in zip(p1, p2))


def dist(p1, p2):
    return sum(abs(v1 - v2) for (v1, v2) in zip(p1, p2))