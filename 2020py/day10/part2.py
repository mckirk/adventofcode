#!/usr/bin/env python3
from collections import defaultdict
from functools import cache
from pathlib import Path
from aocparser import parse

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

spec = "[{i}|\n]"


def main():
    ns = parse(spec, input)

    ns.sort()

    ns = [0] + ns + [ns[-1] + 3]

    verts = defaultdict(set)
    for i, n in enumerate(ns):
        for n2 in ns[i + 1 :]:
            if n2 - n <= 3:
                verts[n2].add(n)
            else:
                break

    @cache
    def ways_to_reach(n2):
        if n2 == 0:
            return 1
        res = sum(ways_to_reach(n) for n in verts[n2])
        return res

    print(ways_to_reach(ns[-1]))


if __name__ == "__main__":
    main()
