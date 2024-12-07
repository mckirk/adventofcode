#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from aocparser import parse
from z3 import *

from lib import *

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

parsed = parse("[{i}: {il}|\n]", input)

def main():
    res = 0
    for result, ns in parsed:
        s = Solver()
        interim = ns[0]
        ops = [Const(f"op{i}", IntSort()) for i in enumerate(ns[1:])]
        for i, n in enumerate(ns[1:]):
            s.add(ops[i] >= 0, ops[i] <= 1)
            interim = If(ops[i] == 0, interim + n, If(ops[i] == 1, interim * n, 0))
        s.add(result == interim)
        if s.check() == sat:
            res += result
    print(res)
    
if __name__ == "__main__":
    main()
