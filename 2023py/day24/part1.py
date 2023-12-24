#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from aocparser import parse
import numpy as np

from lib import *

input_file = Path(__file__).parent / "input.txt"
LOWER_LIM = 200000000000000
UPPER_LIM = 400000000000000

# LOWER_LIM = 7
# UPPER_LIM = 27
# input_file = Path(__file__).parent / "sample1.txt"

input = input_file.read_text().strip()

spec = "[[{i}|,] @ [{si}|,]|\n]"

parsed = parse(spec, input)
parsed_2d = [[VNP.from_val(inner[:2]) for inner in outer] for outer in parsed]
lines = [LineNP.from_pv(*p) for p in parsed_2d]


def main():
    res = 0
    for i1, l1 in enumerate(lines):
        for l2 in lines[i1 + 1 :]:
            i, ts = l1.intersect(l2)
            if i is None:
                continue
            if i.x < LOWER_LIM or i.x > UPPER_LIM:
                continue
            if i.y < LOWER_LIM or i.y > UPPER_LIM:
                continue
            t1, t2 = ts
            # print(l1, l2, t1, t2, i)
            if t1 is None or t1 < 0:
                continue
            if t2 is None or t2 < 0:
                continue
            res += 1
    print(res)


if __name__ == "__main__":
    main()
