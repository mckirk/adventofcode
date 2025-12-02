#!/usr/bin/env python3

from lib import *


def run(inp: Input):
    res = 0
    for f, t in inp.parsed_definite:
        for i in range(f, t+1):
            s = str(i)
            l = len(s)
            if l % 2 == 0 and s[:l >> 1] == s[l >> 1:]:
                res += i
    return res


def main():
    run_on_inputs(run, {1: 1227775554})


if __name__ == "__main__":
    main()
