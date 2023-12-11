#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def adj(pos, limits):
    x, y = pos
    for xd in [-1, 0, 1]:
        x2 = x+xd
        if x2 < 0 or x2 > limits[0]:
            continue

        for yd in [-1, 0, 1]:
            if xd == yd == 0:
                continue

            y2 = y+yd
            if y2 < 0 or y2 > limits[1]:
                continue

            yield (x2, y2)
            

def main():
    seats = set()
    for x, l in enumerate(lines):
        for y, c in enumerate(l):
            if c == "L":
                seats.add((x, y))

    limits = (len(lines[0]), len(lines))

    occ = set()

    while True:
        new_occ = set()
        for s in seats:
            count_occ = sum(p in occ for p in adj(s, limits))
            if s not in occ and count_occ == 0:
                new_occ.add(s)
            if s in occ and count_occ < 4:
                new_occ.add(s)
        
        print(len(occ))
        if occ == new_occ:
            break
        occ = new_occ

            

    pass
    
if __name__ == "__main__":
    main()
