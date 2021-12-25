#!/usr/bin/env python3

import numpy as np
from collections import defaultdict
import re

toy_input = """6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"""

def print_paper(paper: np.ndarray):
    xd, yd = paper.shape

    for y in range(yd):
        print("".join(".#"[dot * 1] for dot in paper[:,y]))


def fold(paper: np.ndarray, instr):
    dir, pos = instr

    axis = {'x':0, 'y':1}[dir]

    fst, _, snd = np.split(paper, [pos, pos+1], axis=axis)
    # print("cut")
    # print_paper(fst)
    # print("---")
    # print_paper(snd)

    max_size = max(fst.shape[axis], snd.shape[axis])
    def pad_part(part):
        part_size = part.shape[axis]

        to_pad = max_size - part_size

        if to_pad == 0: return part
        
        padded_size = list(part.shape)
        padded_size[axis] = max_size

        padded = np.zeros(padded_size, dtype=bool)

        if axis == 0:
            padded[to_pad:,:] = part
        else:
            padded[:,to_pad:] = part

        return padded

    # print(f"{dir}, {pos}: {fst.shape}, {snd.shape}")

    fst = pad_part(fst)

    # print("padded")
    # print_paper(fst)
    # print("---")
    # print_paper(snd)

    flipped = np.flip(snd, axis=axis)
    # print("flipped")
    # print_paper(flipped)

    padded = pad_part(flipped)

    ored = fst | padded
    # print("ored")
    # print_paper(ored)

    return ored

def main():
    with open("./input.txt", "r") as f:
       inp = list(f.read().strip().splitlines())
    # inp = toy_input.splitlines()

    dots, folds = [], []

    x_max, y_max = 0, 0

    for l in inp:
        if l == "": continue

        if l.startswith("fold"):
            m = re.match("fold along (.)=(\d+)", l)

            dir, pos = m.group(1, 2)

            folds.append((dir, int(pos)))
        else:
            x,y = l.split(",")
            x,y = int(x), int(y)

            x_max, y_max = max(x, x_max), max(y, y_max)

            dots.append((x,y))

    paper = np.zeros((x_max+1, y_max+1), dtype=bool)

    for x,y in dots:
        paper[x,y] = True

    # print_paper(paper)

    fold1 = fold(paper, folds[0])

    print(f"Puzzle1: {np.sum(fold1)}")

    for f in folds:
        paper = fold(paper, f)

    print_paper(paper)

    pass

if __name__ == "__main__": main()