#!/usr/bin/env python3
from collections import defaultdict, Counter
from functools import cmp_to_key
from pathlib import Path
from pprint import pprint
from aocparser import parse

from lib import *

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

def main():
    rules = defaultdict(set)
    for l in blocks[0].splitlines():
        b, a = l.split("|")
        rules[b.strip()].add(a.strip())

    res = 0
    for u in blocks[1].splitlines():
        ns = [n.strip() for n in u.split(",")]

        new = sorted(ns, key=cmp_to_key(lambda x, y: 1 if x in rules[y] else -1))

        if new != ns:
            res += int(new[len(new)//2])
    print(res)
    
if __name__ == "__main__":
    main()
