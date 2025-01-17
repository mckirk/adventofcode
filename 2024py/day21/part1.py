#!/usr/bin/env python3

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


def get_seqs(input_seq, shortest_paths):
    res = [[]]
    for i in range(len(input_seq) - 1):
        new_res = []
        for p in shortest_paths[(input_seq[i], input_seq[i + 1])]:
            new_res.extend([r + p + ["A"] for r in res])
        res = new_res
    return res


def get_steps(p):
    steps = 0
    for i in range(len(p) - 1):
        steps += len(dir_shortest_paths[(p[i], p[i + 1])][0]) + 1
    return steps


def print_seq(seq):
    print("".join(readable[x] for x in seq))


def print_seqs(seqs):
    for seq in seqs:
        print_seq(seq)


def run(inp: Input):
    res = 0
    for l in inp.lines:
        seqs = get_seqs("A" + l, num_shortest_paths)
        for _ in range(2):
            seqs = sum(
                [get_seqs(["A"] + list(seq), dir_shortest_paths) for seq in seqs], []
            )
            seqs.sort(key=get_steps)
            seqs = seqs[:1]
        seq = min(seqs, key=len)
        res += int(l[:-1]) * len(seq)

    return res


def main():
    run_on_inputs(run, {1: 126384})


if __name__ == "__main__":
    main()
