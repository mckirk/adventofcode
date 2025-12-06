#!/usr/bin/env python3

from lib import *


def run(inp: Input):
    ls = inp.lines

    nss = [[int(x) for x in l.split()] for l in ls[:-1]]
    ops = [o for o in ls[-1].split()]

    res = 0
    for i, o in enumerate(ops):
        if o == "*":
            sr = 1
            for ns in nss:
                sr *= ns[i]
            res += sr
        else:
            sr = 0
            for ns in nss:
                sr += ns[i]
            res += sr

    return res


def main():
    run_on_inputs(run, {1: 4277556})


if __name__ == "__main__":
    main()
