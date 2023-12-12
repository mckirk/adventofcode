#!/usr/bin/env python3
from collections import defaultdict, Counter
from functools import cache
from pathlib import Path
from pprint import pprint

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()


@cache
def find_ways(cond: str, grps):
    if not cond and grps:
        return set()
    
    if not grps:
        replaced = cond.replace("?", ".")
        if all(x == "." for x in replaced):
            return set([replaced])
        else:
            return set()

    cur_count = grps[0]
    ways = set()
    prefix = ""

    for i, c in enumerate(cond):
        starting, end = cond[i : i + cur_count], cond[i + cur_count :]
        replaced = starting.replace("?", "#")
        end_start = end[0].replace("?", ".") if end else ""
        if (
            len(starting) == cur_count
            and all(x == "#" for x in replaced)
            and end_start != "#"
        ):
            for sw in find_ways(end[1:], grps[1:]):
                ways.add(prefix + replaced + end_start + sw)

        if c == "#":
            break

        prefix += "."

    return ways


def main():
    conds = []
    for l in lines:
        p1, p2 = l.split(" ")
        conds.append((p1, tuple([int(x) for x in p2.split(",")])))

    res = 0

    for cond, grps in conds:
        ways = find_ways(cond, grps)
        print(len(ways))
        res += len(ways)

    print(res)

if __name__ == "__main__":
    main()
