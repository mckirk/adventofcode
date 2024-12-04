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
    res = 0
    for l in lines:
        ds = [int(x) for x in l.split()]
        diff = [y-x for x,y in zip(ds, ds[1:])]
        if all(1 <= d <= 3 for d in diff) or all(1 <= -d <= 3 for d in diff):
            res += 1
    print(res)
    
if __name__ == "__main__":
    main()
