#!/usr/bin/env python3

from lib import *


def run(inp: Input):
    res = 0
    dial = 50

    for rot in inp.parsed_definite:
        if rot.r:
            dial += rot.r
        else:
            dial -= rot.l

        dial = dial % 100

        if dial == 0:
            res += 1

    return res


def main():
    run_on_inputs(run, {1: 3})


if __name__ == "__main__":
    main()
