#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from aocparser import parse
import z3

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

spec = """\
{i}
[{w}|,]\
"""

def mul_inv(a, b):
    """https://rosettacode.org/wiki/Chinese_remainder_theorem#Python_3.6
    """
    b0 = b
    x0, x1 = 0, 1
    if b == 1:
        return 1
    while a > 1:
        q = a // b
        a, b = b, a%b
        x0, x1 = x1 - q * x0, x0
    if x1 < 0:
        x1 += b0
    return x1


def crt(a, n):
    """Chinese Remainder Theorem
    https://rosettacode.org/wiki/Chinese_remainder_theorem#Python_3.6
    """
    s = 0
    prod = 1
    for n_i in n:
        prod *= n_i
    for n_i, a_i in zip(n, a):
        p = prod // n_i
        s += a_i * mul_inv(p, n_i) * p
    return s % prod

def main():
    ts, ids = parse(spec, input)

    a, n = [], []
    for i, id in enumerate(ids):
        if id == "x":
            continue
        id = int(id)
        a.append(id - i)
        n.append(id)

    print(crt(a, n))
    
if __name__ == "__main__":
    main()
