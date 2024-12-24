#!/usr/bin/env python3

from lib import *


MOD = 16777216


def step(x):
    x = (x^(x << 6)) % MOD
    x = (x^(x >> 5)) % MOD
    x = (x^(x << 11)) % MOD
    return x


def stepn(x, n):
    for _ in range(n):
        x = step(x)
    return x


def run(inp: Input):
    res = 0
    for x in inp.parsed:
        res += stepn(x, 2000)
    return res


def main():
    run_on_inputs(run, {1: 37327623})


if __name__ == "__main__":
    main()
