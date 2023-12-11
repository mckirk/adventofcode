#!/usr/bin/env python3
from collections import Counter
from pathlib import Path
from aocparser import parse

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

spec = "[{i}|\n]"


def main():
    ns = parse(spec, input)

    ns.sort()

    ns = [0] + ns + [ns[-1] + 3]

    difs = [ns[i + 1] - ns[i] for i in range(len(ns) - 1)]

    print(difs)

    count = Counter()
    for d in difs:
        count[d] += 1

    print(count[1] * count[3])


if __name__ == "__main__":
    main()
