#!/usr/bin/env python3

from lib import *


def run(inp: Input):
    con = defaultdict(set)
    for f,t in inp.parsed:
        assert f != t
        con[f].add(t)
        con[t].add(f)

    threes = set()
    for a, bs in con.items():
        for b in bs:
            for c in con[b]:
                if c != a and c in bs:
                    threes.add(tuple(sorted([a, b, c])))

    res = 0
    for comps in threes:
        if any(a.startswith("t") for a in comps):
            res += 1
    return res


def main():
    run_on_inputs(run, {1: 7})


if __name__ == "__main__":
    main()
