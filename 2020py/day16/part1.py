#!/usr/bin/env python3

from lib import *


def run(inp: Input):
    rules, yours, others = inp.parsed

    res = 0
    for o in others:
        for n in o:
            for r in rules:
                if r[1] <= n <= r[2] or r[3] <= n <= r[4]:
                    break
            else:
                res += n

    return res


def main():
    run_on_inputs(run, {1: 71})


if __name__ == "__main__":
    main()
