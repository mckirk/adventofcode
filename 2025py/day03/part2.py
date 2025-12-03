#!/usr/bin/env python3

from functools import cache
from lib import *


def find_max(ns: list[int], l: int):
    rs = list(reversed(ns))

    @cache
    def inner(start: int, l: int):
        if start >= len(ns) or not l:
            return 0

        return max(rs[start] + 10*inner(start+1, l-1), inner(start+1, l))
    return inner(0, l)


def run(inp: Input):
    res = 0

    for l in inp.lines:
        bs = [int(c) for c in l]
        res += find_max(bs, 12)

    return res


def main():
    run_on_inputs(run, {1: 3121910778619})


if __name__ == "__main__":
    main()
