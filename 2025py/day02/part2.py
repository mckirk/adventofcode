#!/usr/bin/env python3

from lib import *
import re


factors = {
    2: [(11, 1)],
    3: [(111, 1)],
    4: [(1111, 1), (101, 2)],
    5: [(11111, 1)],
    6: [(111111, 1), (1001, 3), (10101, 2)],
    7: [(1111111, 1)],
    8: [(11111111, 1), (10001, 4), (1010101, 2)],
    9: [(111111111, 1), (1001001, 3)],
    10: [(1111111111, 1), (100001, 5), (101010101, 2)],
}


def get_len10(n: int):
    if n < 100000:
        if n < 1000:
            if n < 10:
                return 1
            elif n < 100:
                return 2
            else:
                return 3
        else:
            if n < 10000:
                return 4
            else:
                return 5
    else:
        if n < 10000000:
            if n < 1000000:
                return 6
            else:
                return 7
        else:
            if n < 100000000:
                return 8
            elif n < 1000000000:
                return 9
            else:
                return 10


def run(inp: Input):
    res = 0
    for f, t in inp.parsed_definite:
        for i in range(f, t+1):
            if i < 10:
                continue
            for f, l in factors[get_len10(i)]:
                if i % f == 0 and (i // f) < 10**l:
                    res += i
                    break
    return res


def run_lazy(inp: Input):
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
