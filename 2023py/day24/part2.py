#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from aocparser import parse
import numpy as np
import z3

from lib import *

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"

input = input_file.read_text().strip()

spec = "[[{i}|,] @ [{si}|,]|\n]"

parsed = parse(spec, input)
parsed_3d = [[VNP.from_val(inner) for inner in outer] for outer in parsed]
lines = [LineNP.from_pv(*p) for p in parsed_3d]


def main():
    rp = [z3.Int(f"rp{i}") for i in range(3)]
    rv = [z3.Int(f"rv{i}") for i in range(3)]
    ts = [z3.Int(f"t{i}") for i, _ in enumerate(lines)]

    constraints = []
    for i, line in enumerate(lines):
        for d in range(3):
            constraints.append(rp[d] + ts[i] * rv[d] == line.p[d] + ts[i] * line.v[d])
        constraints.append(ts[i] >= 0)

    solver = z3.Solver()
    solver.add(constraints)
    print(solver.check())
    model = solver.model()
    res = 0
    for i in range(3):
        print(model[rp[i]], model[rv[i]])
        res += model.eval(rp[i]).as_long()

    print(res)


if __name__ == "__main__":
    main()
