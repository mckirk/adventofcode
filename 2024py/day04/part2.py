#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from aocparser import parse

from lib import *

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

factors = [
    (1,1), (1,-1),
    (-1,1), (-1,-1),
]

def scan(ls):
    part = set()
    count = 0
    for y, l in enumerate(ls):
        for x, c in enumerate(l):
            if c != "A":
                continue

            local_count = 0
            for xf, yf in factors:
                if not xf and not yf:
                    continue
                
                try:
                    for i in range(-1, 2):
                        if y+yf*i < 0:
                            break
                        if x+xf*i < 0:
                            break
                        if not ls[y+yf*i][x+xf*i] == "ASM"[i]:
                            break
                    else:
                        local_count += 1
                except:
                    pass

            if local_count == 2:
                count += 1

    return count

def main():
    print(scan(lines))
    
if __name__ == "__main__":
    main()
