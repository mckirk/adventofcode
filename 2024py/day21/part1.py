#!/usr/bin/env python3

from functools import cache
from lib import *
import math


# 789
# 456
# 123
#  0A

num_kp = {
    '7': V(0, 0),
    '8': V(1, 0),
    '9': V(2, 0),
    '4': V(0, 1),
    '5': V(1, 1),
    '6': V(2, 1),
    '1': V(0, 2),
    '2': V(1, 2),
    '3': V(2, 2),
    '0': V(1, 3),
    'A': V(2, 3),
}

dir_kp = {
    V(0, -1): V(1, 0),
    V(-1, 0): V(0, 1),
    V(0, 1): V(1, 1),
    V(1, 0): V(2, 1),
    'A': V(2, 0)
}

readable = {
    V(0, -1): '^',
    V(-1, 0): '<',
    V(0, 1): 'v',
    V(1, 0): '>',
    'A': 'A'
}


def sgn(x):
    return 0 if x == 0 else (-1 if x < 0 else 1)

# @cache
def find_kp_seq(pos_seq: tuple[V], start_pos: V) -> list:
    if len(pos_seq) == 0:
        return []
    
    head = pos_seq[0]
    tail = pos_seq[1:]

    delta = head - start_pos
    dir_x = V(sgn(delta.x), 0)
    dir_y = V(0, sgn(delta.y))
    # dir_pos = [dir_kp[dir] for dir in [dir_x, dir_y] if dir != V(0, 0)]

    seq = []
    if dir_x != V(0, 0):
        seq += [dir_x]*abs(delta.x)
    if dir_y != V(0, 0):
        seq += [dir_y]*abs(delta.y)
    seq += ['A']
    
    return seq + find_kp_seq(tail, head)


def run(inp: Input):
    res = 0
    for l in inp.lines:
        pos_seq = tuple(num_kp[c] for c in l)
        for _ in range(3):
            print(pos_seq)
            seq = find_kp_seq(pos_seq, num_kp['A'])
            print("".join(readable[x] for x in seq))
            pos_seq = [dir_kp[x] for x in seq]
        res += int(l[:-1])*len(seq)

    return res

def main():
    run_on_inputs(run, {1: 126384})


if __name__ == "__main__":
    main()
