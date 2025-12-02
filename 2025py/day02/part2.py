#!/usr/bin/env python3

from lib import *
import re


def run(inp: Input):
    res = 0
    for f, t in inp.parsed_definite:
        for i in range(f, t+1):
            s = str(i)
            if re.match(r"^(\w+)\1+$", s):
                res += i
    return res


def main():
    run_on_inputs(run, {1: 4174379265})


if __name__ == "__main__":
    main()
