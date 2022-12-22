#!/usr/bin/env python3

from os import path
import re


blocked = set()
in_map = set()
start_point = None

teleport1 = dict()
blocked_teleport1 = set()

with open(path.join(path.dirname(__file__), "input.txt")) as f:
    ls = f.readlines()
    map_part = ls[:-2]
    instrs = [(int(m[0]), m[1])
              for m in re.findall(r"(\d+)([RL_])", ls[-1].strip() + "_")]

max_len = max(len(l[:-1]) for l in map_part)
padded = [f"{l[:-1]:<{max_len}}" for l in map_part]

for y, l in enumerate(padded):
    map_parts = re.match(r"( *)([.#]+)( *)", l).groups()

    dims = list(map(len, map_parts))
    start = dims[0]
    end = dims[0]+dims[1]

    if start_point is None:
        start_point = (start, 0)

    mid = map_parts[1]

    for x_rel, c in enumerate(mid):
        x = start+x_rel
        if c == "#":
            blocked.add((x, y))
        in_map.add((x, y))

    if mid[0] != "#" and mid[-1] != "#":
        teleport1[((start-1, y), 2)] = ((end-1, y), None)
        teleport1[((end, y), 0)] = ((start, y), None)
    else:
        blocked_teleport1.add(((start-1, y), 2))
        blocked_teleport1.add(((end, y), 0))

transposed = ["".join(cs) for cs in zip(*[[c for c in l] for l in padded])]

for x, l in enumerate(transposed):
    map_parts = re.match(r"( *)([.#]+)( *)", l).groups()

    dims = list(map(len, map_parts))
    start = dims[0]
    end = dims[0]+dims[1]

    mid = map_parts[1]

    for y_rel, c in enumerate(mid):
        y = start+y_rel
        if c == "#":
            assert (x, y) in blocked
        assert (x, y) in in_map

    if mid[0] != "#" and mid[-1] != "#":
        teleport1[((x, start-1), 3)] = ((x, end-1), None)
        teleport1[((x, end), 1)] = ((x, start), None)
    else:
        blocked_teleport1.add(((x, start-1), 3))
        blocked_teleport1.add(((x, end), 1))


teleport2 = dict()
blocked_teleport2 = set()


dirs = [(1, 0), (0, 1), (-1, 0), (0, -1)]
turns = {"R": 1, "L": -1, "_": 0}

TOP = 0
RIGHT = 1
BOT = 2
LEFT = 3


def add(p1, p2):
    return tuple(a+b for a, b in zip(p1, p2))


def region_side(r, s):
    rx, ry = cube_regions[r-1]
    if s == TOP:
        p = (rx, ry)
        dir = (1, 0)
        dir_in, dir_out = 1, 3
    elif s == RIGHT:
        p = (rx+cube_dim-1, ry)
        dir = (0, 1)
        dir_in, dir_out = 2, 0
    elif s == BOT:
        p = (rx, ry+cube_dim-1)
        dir = (1, 0)
        dir_in, dir_out = 3, 1
    elif s == LEFT:
        p = (rx, ry)
        dir = (0, 1)
        dir_in, dir_out = 0, 2

    out = dirs[dir_out]

    for _ in range(cube_dim):
        np = add(p, out)
        yield (p, np, dir_in, dir_out)
        p = add(p, dir)


def connect(r1, s1, r2, s2, rev):
    mr = reversed if rev else lambda x: x

    for (p1, np1, dir_in1, dir_out1), (p2, np2, dir_in2, dir_out2) in zip(region_side(r1, s1), mr(list(region_side(r2, s2)))):
        if p1 not in blocked and p2 not in blocked:
            teleport2[(np1, dir_out1)] = (p2, dir_in2)
            teleport2[(np2, dir_out2)] = (p1, dir_in1)
        else:
            blocked_teleport2.add((np1, dir_out1))
            blocked_teleport2.add((np2, dir_out2))


TEST = False

if TEST:
    #         a11b
    #         1111
    #         1111
    #         d11c
    # b22aa33dd44c
    # 222233334444
    # 222233334444
    # h22gg33ff44e
    #         f55ee66c
    #         55556666
    #         55556666
    #         g55hh66b

    cube_dim = len(map_part) // 3
    assert len(map_part) == 3*cube_dim

    cube_regions = [(cx*cube_dim, cy*cube_dim) for cx, cy in [
        (2, 0), (0, 1), (1, 1), (2, 1), (2, 2), (3, 2)
    ]]

    connect(1, LEFT, 3, TOP, False)
    connect(3, BOT, 5, LEFT, True)
    connect(4, RIGHT, 6, TOP, True)

    connect(1, TOP, 2, TOP, True)
    connect(1, RIGHT, 6, RIGHT, True)
    connect(6, BOT, 2, LEFT, True)
    connect(5, BOT, 2, BOT, True)
else:
    #     a1b|b2c
    #     111|222
    #     f1e|e2d
    #     ---
    #     f3e
    #     333
    #     g3d
    #     ---
    # f4g|g5d
    # 444|555
    # a4h|h5c
    # ---
    # a6h
    # 666
    # b6c

    cube_dim = len(map_part) // 4
    assert len(map_part) == 4*cube_dim

    cube_regions = [(cx*cube_dim, cy*cube_dim) for cx, cy in [
        (1, 0), (2, 0), (1, 1), (0, 2), (1, 2), (0, 3)
    ]]

    connect(1, TOP, 6, LEFT, False)
    connect(2, TOP, 6, BOT, False)
    connect(2, RIGHT, 5, RIGHT, True)
    connect(2, BOT, 3, RIGHT, False)
    connect(5, BOT, 6, RIGHT, False)
    connect(4, LEFT, 1, LEFT, True)
    connect(4, TOP, 3, LEFT, False)

def sanity():
    assert len(teleport2.keys() & blocked_teleport2) == 0

    for r in range(1, 7):
        for s in range(4):
            for p, np, dir_in, dir_out in region_side(r, s):
                assert p in in_map
                assert np in in_map or (np, dir_out) in (
                    teleport2.keys() | blocked_teleport2)


def follow_instrs(blocked, teleport, blocked_teleport):
    p = start_point
    d = 0

    def move_forward():
        nonlocal p, d
        dir = dirs[d]

        new_pos = add(p, dir)
        if new_pos in blocked:
            return

        if (new_pos, d) in blocked_teleport:
            return

        maybe_tel = teleport.get((new_pos, d))
        new_dir = None
        if maybe_tel is not None:
            new_pos, new_dir = maybe_tel

        p = new_pos
        if new_dir is not None:
            d = new_dir

        assert p in in_map

    for dist, turn in instrs:
        for _ in range(dist):
            move_forward()

        d += turns[turn]
        d %= 4

    col, row = p

    print(1000 * (row + 1) + 4 * (col + 1) + d)

sanity()

follow_instrs(blocked, teleport1, blocked_teleport1)
follow_instrs(blocked, teleport2, blocked_teleport2)
