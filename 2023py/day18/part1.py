#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint

from lib import *

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

dir_transl = dict(U=Dir.N, R=Dir.E, L=Dir.W, D=Dir.S)


def main():
    ins = []
    for l in lines:
        ins.append(l.split())

    dug = set()
    pos = V(0, 0)
    dug.add(pos)

    for d, l, _ in ins:
        l = int(l)
        d = dir_transl[d]

        for _ in range(l):
            pos += d.value
            dug.add(pos)

    min_x, min_y = min(p.x for p in dug), min(p.y for p in dug)
    dug = {p - V(min_x, min_y) for p in dug}

    limits = max(p.x for p in dug) + 1, max(p.y for p in dug) + 1

    # for y in range(limits[1]):
    #     for x in range(limits[0]):
    #         if V(x, y) in dug:
    #             print("#", end="")
    #         else:
    #             print(".", end="")
    #     print()

    not_dug: set[V] = set()
    for x in range(limits[0]):
        not_dug.add(V(x, -1))
        not_dug.add(V(x, limits[1]))

    for y in range(limits[1]):
        not_dug.add(V(-1, y))
        not_dug.add(V(limits[0], y))

    init_not_dug = len(not_dug)

    while True:
        new_not_dug = set()
        for p in not_dug:
            for p2, _ in p.adj(limits):
                if p2 not in dug and p2 not in not_dug:
                    new_not_dug.add(p2)
        if not new_not_dug:
            break

        not_dug |= new_not_dug

    # print()

    # for y in range(limits[1]):
    #     for x in range(limits[0]):
    #         if V(x, y) in not_dug:
    #             print(".", end="")
    #         else:
    #             print("#", end="")
    #     print()

    print(limits[0] * limits[1] - (len(not_dug) - init_not_dug))


if __name__ == "__main__":
    main()
