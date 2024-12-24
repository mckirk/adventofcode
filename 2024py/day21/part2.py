#!/usr/bin/env python3

from functools import cache
from lib import *

num_kp = {
    "7": V(0, 0),
    "8": V(1, 0),
    "9": V(2, 0),
    "4": V(0, 1),
    "5": V(1, 1),
    "6": V(2, 1),
    "1": V(0, 2),
    "2": V(1, 2),
    "3": V(2, 2),
    "0": V(1, 3),
    "A": V(2, 3),
}

dir_kp = {
    V(0, -1): V(1, 0),
    V(-1, 0): V(0, 1),
    V(0, 1): V(1, 1),
    V(1, 0): V(2, 1),
    "A": V(2, 0),
}

readable = {V(0, -1): "^", V(-1, 0): "<", V(0, 1): "v", V(1, 0): ">", "A": "A"}


def sgn(x):
    if x == 0:
        return 0
    if x < 0:
        return -1
    return 1


def shortest_paths(kp, start, goal):
    if start == goal:
        return [[]]

    start_pos = kp[start]
    goal_pos = kp[goal]

    delta = goal_pos - start_pos
    sx, sy = sgn(delta.x), sgn(delta.y)
    dx, dy = V(sx, 0), V(0, sy)
    mx, my = abs(delta.x), abs(delta.y)

    if sx == 0:
        return [[dy] * my]
    if sy == 0:
        return [[dx] * mx]

    paths = []
    if V(start_pos.x, goal_pos.y) in kp.values():
        paths.append([dy] * my + [dx] * mx)
    if V(goal_pos.x, start_pos.y) in kp.values():
        paths.append([dx] * mx + [dy] * my)

    return paths


num_shortest_paths = {
    (k1, k2): shortest_paths(num_kp, k1, k2)
    for k1 in num_kp.keys()
    for k2 in num_kp.keys()
}

dir_shortest_paths = {
    (k1, k2): shortest_paths(dir_kp, k1, k2)
    for k1 in dir_kp.keys()
    for k2 in dir_kp.keys()
}

NUM = 0
DIR = 1


@cache
def get_seq_len(kp_sel, start, seq, lvl_to_go):
    if lvl_to_go == 0:
        return len(seq)

    kp = dir_shortest_paths if kp_sel == DIR else num_shortest_paths

    res = 0
    from_start = [start] + list(seq)
    for i in range(len(seq)):
        res += min(
            get_seq_len(DIR, "A", tuple(p + ["A"]), lvl_to_go - 1)
            for p in kp[(from_start[i], from_start[i + 1])]
        )
    return res


def run(inp: Input):
    res = 0
    for l in inp.lines:
        seq_len = get_seq_len(NUM, "A", tuple(l), 26)
        res += int(l[:-1]) * seq_len

    return res


def main():
    run_on_inputs(run, {1: None})


if __name__ == "__main__":
    main()
