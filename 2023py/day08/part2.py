#!/usr/bin/env python3
from pathlib import Path

from math import gcd
from functools import reduce

def lcm(a, b):
    return a * b // gcd(a, b)

def lcmm(*args):
    return reduce(lcm, args)

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    d = lines[0]

    nodes = dict()
    for l in lines[2:]:
        p1, p2 = l.split(" = ")
        p2_1, p2_2 = p2.split(", ")

        nodes[p1] = [p2_1[1:], p2_2[:-1]]

    start = set()

    for k in nodes.keys():
        if k[-1] == "A":
            start.add(k)

    cycles = set()

    for cur in start:
        steps = 0
        while cur [-1] != "Z":
            cur_d = d[steps % len(d)]
            cur_n = nodes[cur]
            if cur_d == "L":
                cur = cur_n[0]
            elif cur_d == "R":
                cur = cur_n[1]
            
            steps += 1

        cycles.add(steps)

    print(lcmm(*cycles))
    
if __name__ == "__main__":
    main()
