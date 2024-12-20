#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from aocparser import parse

from lib import *

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

spec = """\
[{key:w}\\{[<more:{var:w}\\>{v:i}:{dst:w}|less:{var:w}\\<{v:i}:{dst:w}|otherwise:{dst:w}>|,]\\}|\n]

[\\{[{key:w}={i}|,]\\}|\n]"""


def main():
    rules, ratings = parse(spec, input)

    def is_accepted(rating, workflow):
        if workflow == "A":
            return True
        if workflow == "R":
            return False

        for ins in rules[workflow]:
            if ins.less and rating[ins.less.var] < ins.less.v:
                return is_accepted(rating, ins.dst)
            if ins.more and rating[ins.more.var] > ins.more.v:
                return is_accepted(rating, ins.dst)
            if ins.otherwise:
                return is_accepted(rating, ins.dst)

    res = 0
    for r in ratings:
        if is_accepted(r, "in"):
            res += sum(r.values())

    print(res)


if __name__ == "__main__":
    main()
