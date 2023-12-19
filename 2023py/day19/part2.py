#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from aocparser import parse

from lib import *

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

spec = """\
[{key:w}`{[<more:{var:w}`>{v:i}:{dst:w}|less:{var:w}`<{v:i}:{dst:w}|otherwise:{dst:w}>|,]`}|\n]

[`{[{key:w}={i}|,]`}|\n]"""


FULL_RANGE = Range(1, 4001)


def main():
    rules, ratings = parse(spec, input)

    def accepted_ranges(rating_ranges, workflow):
        if workflow == "A":
            return [rating_ranges]
        if workflow == "R":
            return []

        cur_ranges = dict(rating_ranges)

        accepted = []

        for ins in rules[workflow]:
            if ins.otherwise:
                accepted += accepted_ranges(cur_ranges, ins.dst)
                return accepted

            if ins.less:
                range_yes, range_no = FULL_RANGE.split(ins.less.v)
                var = ins.less.var

            if ins.more:
                range_no, range_yes = FULL_RANGE.split(ins.more.v + 1)
                var = ins.more.var

            range_yes = cur_ranges[var].intersect(range_yes)
            range_no = cur_ranges[var].intersect(range_no)

            if range_yes:
                new_ranges = dict(cur_ranges)
                new_ranges[var] = range_yes
                accepted += accepted_ranges(new_ranges, ins.dst)

            if not range_no:
                return accepted

            cur_ranges[var] = range_no

        assert False

    ranges = accepted_ranges({c: FULL_RANGE for c in "xmas"}, "in")
    res = 0
    for rv in ranges:
        this = 1
        for r in rv.values():
            this *= len(r)
        res += this

    print(res)


if __name__ == "__main__":
    main()
