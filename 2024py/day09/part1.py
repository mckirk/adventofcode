#!/usr/bin/env python3
from collections import defaultdict, Counter

from lib import *

sample1_expected = 1928
sample2_expected = None


def run(inp: Input):
    pos_stack = []
    free = []

    file_id = 0
    pos = 0
    in_free = False
    for c in inp.lines[0]:
        n = int(c)
        if in_free:
            free += range(pos, pos+n)
        else:
            for i in range(n):
                pos_stack.append((pos+i, file_id))
            file_id += 1
        in_free = not in_free
        pos += n

    free = list(reversed(free))
    moved = []
    while free:
        p = free.pop()
        (pp, id) = pos_stack.pop()

        if p > pp:
            pos_stack.append((pp, id))
            break

        moved.append((p, id))

    res = 0
    for p, id in pos_stack + moved:
        res += p*id
    return res


def main():
    run_on_inputs(sample1_expected, sample2_expected, run)


if __name__ == "__main__":
    main()
