#!/usr/bin/env python3

from os import path


with open(path.join(path.dirname(__file__), "input.txt")) as f:
    ns_orig = [[i, int(l)] for i, l in enumerate(f.readlines())]


def do(premul, times):
    ns = [[i, premul*n] for i, n in ns_orig]
    num = len(ns)

    def mix():
        for nl_orig in ns:
            from_i, n = nl_orig

            to_i = (from_i + n) % (num - 1)

            if n < 0 and to_i == 0:
                to_i = num - 1

            for nl in ns:
                old_i = nl[0]
                if old_i == from_i:
                    continue

                if to_i > from_i:
                    if old_i > from_i and old_i <= to_i:
                        nl[0] -= 1
                elif from_i > to_i:
                    if old_i >= to_i and old_i < from_i:
                        nl[0] += 1

            nl_orig[0] = to_i

    for _ in range(times):
        mix()

    final_ns = [nl[1] for nl in sorted(ns)]

    zero_idx = final_ns.index(0)

    s = sum(final_ns[(i+zero_idx) % num] for i in [1000, 2000, 3000])
    print(s)


do(1, 1)
do(811589153, 10)
