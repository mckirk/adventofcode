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

def count(l):
    c = Counter()
    for x in l:
        c[x] += 1
    return c

def main():
    l1, l2 = [list(t) for t in zip(*[[int(n) for n in l.split()] for l in lines])]
    c1, c2 = count(l1), count(l2)

    res = 0
    for n1, c in c1.items():
        res += n1*c*c2[n1]

    print(res)
    
if __name__ == "__main__":
    main()
