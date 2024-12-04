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

def main():
    l1, l2 = [list(t) for t in zip(*[[int(n) for n in l.split()] for l in lines])]
    l1.sort()
    l2.sort()

    res = 0
    for n1, n2 in zip(l1, l2):
        res += abs(n1 - n2)

    print(res)
    
if __name__ == "__main__":
    main()
