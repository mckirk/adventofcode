#!/usr/bin/env python3

from lib import *


def tuple_to_int(t):
    r = 0
    for n in t:
        if n == " ":
            continue
        r *= 10
        r += int(n)
    return r


def run(inp: Input):
    ls = inp.lines

    css = [[x for x in l] for l in ls[:-1]]
    css = list(zip(*css))

    ns = [tuple_to_int(t) for t in css]
    nss = []
    cur = []
    for n in ns:
        if n == 0:
            nss.append(cur)
            cur = []
        else:
            cur.append(n)
    nss.append(cur)

    ops = [o for o in ls[-1].split()]

    res = 0
    for o, ns in zip(ops, nss):
        if o == "*":
            sr = 1
            for n in ns:
                sr *= n
            res += sr
        else:
            sr = 0
            for n in ns:
                sr += n
            res += sr

    return res


def main():
    run_on_inputs(run, {1: 3263827}, raw_input=True)


if __name__ == "__main__":
    main()
