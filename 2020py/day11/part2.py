#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint

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


def adj(pos, limits):
    x, y = pos
    for (xd, yd) in directions():
        x2 = x+xd
        y2 = y+yd
        if x2 < 0 or x2 > limits[0]:
            continue
        if y2 < 0 or y2 > limits[1]:
            continue
        yield (x2, y2)


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
            

def main():
    seats = set()
    for y, l in enumerate(lines):
        for x, c in enumerate(l):
            if c == "L":
                seats.add((x, y))

    limits = (len(lines[0]), len(lines))

    seen = dict()
    for s in seats:
        for d in directions():
            for s2 in line(s, d, limits):
                if s2 in seats:
                    seen[(s, d)] = s2
                    break

    pprint(seen)

    occ = set()

    while True:
        new_occ = set()
        for s in seats:
            count_occ = 0
            for d in directions():
                s2 = seen.get((s, d))
                if s2 is not None and s2 in occ:
                    count_occ += 1
            
            if s not in occ and count_occ == 0:
                new_occ.add(s)
            if s in occ and count_occ < 5:
                new_occ.add(s)
        
        print(len(occ))
        if occ == new_occ:
            break
        occ = new_occ

            

    pass
    
if __name__ == "__main__":
    main()
