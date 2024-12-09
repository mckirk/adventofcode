#!/usr/bin/env python3
from collections import defaultdict, Counter

from lib import *

sample1_expected = 2858
sample2_expected = None


def run(inp: Input):
    pos_stack = []
    free = dict()

    file_id = 0
    pos = 0
    in_free = False
    for c in inp.lines[0]:
        n = int(c)
        if in_free:
            free[pos] = n
        else:
            pos_stack.append((pos, n, file_id))
            file_id += 1
        in_free = not in_free
        pos += n
    
    moved = []
    while pos_stack:
        (pos, n, file_id) = pos_stack.pop()
        for (free_pos, free_n) in sorted(free.items()):
            if free_n >= n and free_pos < pos:
                moved.append((free_pos, n, file_id))
                del free[free_pos]
                rem = free_n - n
                if rem:
                    free[free_pos+n] = rem
                break
        else:
            moved.append((pos, n, file_id))

    res = 0
    for (pos, n, file_id) in pos_stack + moved:
        for p in range(pos, pos+n):
            res += p*file_id
    return res


def main():
    run_on_inputs(sample1_expected, sample2_expected, run)


if __name__ == "__main__":
    main()
