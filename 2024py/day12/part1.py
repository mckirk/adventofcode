#!/usr/bin/env python3
from collections import defaultdict, Counter

from lib import *

sample1_expected = 140
sample2_expected = 1930


dirs = [Dir.N, Dir.S, Dir.E, Dir.W]


def run(inp: Input):
    res = 0
    unassigned = {V(*p): c for p, c in inp.as_pos}
    while unassigned:
        s, c = unassigned.popitem()

        plot = {s}
        todo = [s]
        while todo:
            t = todo.pop()
            for d in dirs:
                op = t + d.value
                if op in plot:
                    continue
                oc = unassigned.get(op)
                if oc == c:
                    plot.add(op)
                    todo.append(op)
                    del unassigned[op]

        peri = 0
        for p in plot:
            for d in dirs:
                op = p + d.value
                if op not in plot:
                    peri += 1

        res += len(plot) * peri

    return res


def main():
    run_on_inputs(sample1_expected, sample2_expected, run)


if __name__ == "__main__":
    main()
