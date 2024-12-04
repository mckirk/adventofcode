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

def safe(ds):
    diff = [y-x for x,y in zip(ds, ds[1:])]
    if all(1 <= d <= 3 for d in diff) or all(1 <= -d <= 3 for d in diff):
        return True
    else:
        return False

def main():
    res = 0
    for l in lines:
        ds = [int(x) for x in l.split()]
        if safe(ds):
            res += 1
        else:
            for i, _ in enumerate(ds):
                if safe(ds[:i] + ds[1+i:]):
                    res += 1
                    break
        
    print(res)
    
if __name__ == "__main__":
    main()
