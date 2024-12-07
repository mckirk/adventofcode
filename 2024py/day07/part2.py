#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from aocparser import parse
from z3 import *

from lib import *

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

parsed = parse("[{i}: {il}|\n]", input)

def has_result(exp, ns):
    res = {ns[0]}
    for n in ns[1:]:
        new_res = set()
        for r in res:
            for p in [
                r + n,
                r * n,
                int(str(r) + str(n))
            ]:
                if p <= exp:
                    new_res.add(p)
        res = new_res
    return exp in res

def main():
    res = 0
    for result, ns in parsed:
        if has_result(result, ns):
            res += result
    print(res)
    
if __name__ == "__main__":
    main()
