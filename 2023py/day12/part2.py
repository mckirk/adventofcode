#!/usr/bin/env python3
from collections import defaultdict, Counter
from functools import cache
from pathlib import Path
from pprint import pprint

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()


@cache
def find_ways(cond: str, grps):
    if not cond and grps:
        return 0

    if not grps:
        if all(x in "?." for x in cond):
            return 1
        else:
            return 0

    cur_count = grps[0]
    ways = 0

    for i, c in enumerate(cond):
        starting, end = cond[i : i + cur_count], cond[i + cur_count :]
        if (
            len(starting) == cur_count
            and all(x in "?#" for x in starting)
            and end[:1] != "#"
        ):
            ways += find_ways(end[1:], grps[1:])

        if c == "#":
            break

    return ways


def main():
    conds = []
    for l in lines:
        p1, p2 = l.split(" ")
        p2_s = tuple([int(x) for x in p2.split(",")])
        conds.append(("?".join([p1] * 5), p2_s * 5))

    res = 0

    for cond, grps in conds:
        ways = find_ways(cond, grps)
        res += ways

    print(res)


if __name__ == "__main__":
    main()
