#!/usr/bin/env python3

from os import path

elves = set()

with open(path.join(path.dirname(__file__), "input.txt")) as f:
    ls = f.readlines()
    for y, l in enumerate(ls):
        for x, c in enumerate(l):
            if c == "#":
                elves.add((x, y))


local_d, global_d = 0, 0


def propose(elf):
    global local_d, elves
    x, y = elf

    res = None

    other_pos = [
        (x, y-1), (x+1, y-1), (x-1, y-1),
        (x, y+1), (x+1, y+1), (x-1, y+1),
        (x-1, y), (x-1, y+1), (x-1, y-1),
        (x+1, y), (x+1, y+1), (x+1, y-1)
    ]

    others = [p not in elves for p in other_pos]

    if all(others):
        return None

    if local_d == 0 and all(others[:3]):
        res = (x, y-1)
    if local_d == 1 and all(others[3:6]):
        res = (x, y+1)
    if local_d == 2 and all(others[6:9]):
        res = (x-1, y)
    if local_d == 3 and all(others[9:]):
        res = (x+1, y)

    local_d += 1
    local_d %= 4

    return res


def one_round():
    global local_d, global_d, elves
    proposals = dict()

    for e in elves:
        local_d = global_d
        for _ in range(4):
            prop = propose(e)
            if prop is not None:
                break
        else:
            continue
        if prop in proposals:
            proposals[prop] = None
        else:
            proposals[prop] = e

    moved = False
    for prop, e in proposals.items():
        if e is not None:
            elves.remove(e)
            elves.add(prop)

            moved = True

    global_d += 1
    global_d %= 4

    return moved


def rect():
    mins, maxs = [None, None], [None, None]

    for x, y in elves:
        if mins[0] is None or x < mins[0]:
            mins[0] = x
        if mins[1] is None or y < mins[1]:
            mins[1] = y
        if maxs[0] is None or x > maxs[0]:
            maxs[0] = x
        if maxs[1] is None or y > maxs[1]:
            maxs[1] = y

    return mins, maxs


def rect_empty():
    mins, maxs = rect()
    print((maxs[0] - mins[0] + 1) * (maxs[1] - mins[1] + 1) - len(elves))


def print_board():
    mins, maxs = rect()

    for y in range(mins[1], maxs[1]+1):
        l = ""
        for x in range(mins[0], maxs[0]+1):
            if (x, y) in elves:
                l += "#"
            else:
                l += "."
        print(l)


i = 0

moved = True
while moved:
    moved = one_round()
    i += 1

    if i == 10:
        rect_empty()

print(i)
