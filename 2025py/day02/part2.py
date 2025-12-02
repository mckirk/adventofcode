#!/usr/bin/env python3

from lib import *
import re


def does_repeat(n: int):
    if n < 100000:
        if n < 1000:
            if n < 10:
                return False
            elif n < 100:
                return n % 11 == 0 and (n // 11) < 10
            else:
                return n % 111 == 0 and (n // 111) < 10
        else:
            if n < 10000:
                return (n % 1111 == 0 and (n // 1111) < 10) or (
                    n % 101 == 0 and (n // 101) < 100
                )
            else:
                return n % 11111 == 0 and (n // 11111) < 10
    else:
        if n < 10000000:
            if n < 1000000:
                return (
                    (n % 111111 == 0 and (n // 111111) < 10)
                    or (n % 10101 == 0 and (n // 10101) < 100)
                    or (n % 1001 == 0 and (n // 1001) < 1000)
                )
            else:
                return n % 1111111 == 0 and (n // 1111111) < 10
        else:
            if n < 100000000:
                return (
                    (n % 11111111 == 0 and (n // 11111111) < 10)
                    or (n % 1010101 == 0 and (n // 1010101) < 100)
                    or (n % 10001 == 0 and (n // 10001) < 10000)
                )
            elif n < 1000000000:
                return (n % 111111111 == 0 and (n // 111111111) < 10) or (
                    n % 1001001 == 0 and (n // 1001001) < 1000
                )
            else:
                return (
                    (n % 1111111111 == 0 and (n // 1111111111) < 10)
                    or (n % 101010101 == 0 and (n // 101010101) < 100)
                    or (n % 100001 == 0 and (n // 100001) < 100000)
                )


def run(inp: Input):
    res = 0
    for f, t in inp.parsed_definite:
        for i in range(f, t + 1):
            if does_repeat(i):
                res += i
    return res


def run_lazy(inp: Input):
    res = 0
    for f, t in inp.parsed_definite:
        for i in range(f, t + 1):
            s = str(i)
            if re.match(r"^(\w+)\1+$", s):
                res += i
    return res


def main():
    run_on_inputs(run, {1: 4174379265})


if __name__ == "__main__":
    main()
