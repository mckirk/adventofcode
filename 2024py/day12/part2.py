#!/usr/bin/env python3
from collections import defaultdict, Counter
import itertools

from lib import *

sample1_expected = 80
sample2_expected = 1206


dirs = [Dir.N, Dir.S, Dir.E, Dir.W]
dirs_angle = set(Dir) - set(dirs)


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

        peri = set()
        for p in plot:
            for d in dirs:
                op = p + d.value
                if op not in plot:
                    peri.add((p, d))

        sides = 0
        while peri:
            p, d = peri.pop()
            sides += 1

            ns = [Dir.N, Dir.S]
            ew = [Dir.E, Dir.W]

            orth = {Dir.N: ew, Dir.S: ew, Dir.E: ns, Dir.W: ns}

            todo = [p]

            while todo:
                t = todo.pop()
                for nd in orth[d]:
                    op = t + nd.value
                    if (op, d) in peri:
                        peri.remove((op, d))
                        todo.append(op)

        res += len(plot) * sides

    return res


def main():
    run_on_inputs(sample1_expected, sample2_expected, run)


if __name__ == "__main__":
    main()
