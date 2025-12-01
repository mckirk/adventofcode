#!/usr/bin/env python3

from lib import *


def run(inp: Input):
    res = 0
    dial = 50

    for rot in inp.parsed_definite:
        if rot.r:
            for i in range(rot.r):
                dial += 1
                dial = dial % 100
                if dial == 0:
                    res += 1
        else:
            for i in range(rot.l):
                dial -= 1
                dial = dial % 100
                if dial == 0:
                    res += 1


    return res


def main():
    run_on_inputs(run, {1: 6})


if __name__ == "__main__":
    main()
