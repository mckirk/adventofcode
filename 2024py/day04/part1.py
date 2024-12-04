#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from aocparser import parse

from lib import *

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

factors = [
    (1,0), (-1,0),
    (0,1), (0,-1),
    (1,1), (1,-1),
    (-1,1), (-1,-1),
]

def scan(ls):
    part = set()
    count = 0
    for y, l in enumerate(ls):
        for x, c in enumerate(l):
            if c != "X":
                continue

            for xf, yf in factors:
                if not xf and not yf:
                    continue
                
                try:
                    for i in range(4):
                        if y+yf*i < 0:
                            break
                        if x+xf*i < 0:
                            break
                        if not ls[y+yf*i][x+xf*i] == "XMAS"[i]:
                            break
                    else:
                        for i in range(4):
                            part.add((y+yf*i, x+xf*i))
                        count += 1
                except:
                    pass

    # out = []
    # for y, l in enumerate(ls):
    #     l = list(l)
    #     for x, c in enumerate(l):
    #         if (y, x) not in part:
    #             l[x] = "."
    #     out.append("".join(l))

    # print("\n".join(out))

    return count

def main():
    print(scan(lines))
    
if __name__ == "__main__":
    main()
