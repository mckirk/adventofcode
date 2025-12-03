#!/usr/bin/env python3

from lib import *


def run(inp: Input):
    res = 0

    for l in inp.lines:
        bs = [int(c) for c in l]
        m = 0
        for a, xa in enumerate(bs):
            for b, xb in enumerate(bs[a+1:]):
                m = max(m, 10*xa+xb)
        res += m

    return res


def main():
    run_on_inputs(run, {1: 357})


if __name__ == "__main__":
    main()
