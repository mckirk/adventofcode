#!/usr/bin/env python3

from lib import *


def run(inp: Input):
    keys = []
    locks = []

    for b in inp.blocks:
        ls = b.splitlines()
        if b[0] == "#":
            lock = []
            for i in range(5):
                for j in range(6):
                    if ls[j+1][i] == ".":
                        lock.append(j)
                        break
            locks.append(tuple(lock))
        else:
            key = []
            for i in range(5):
                for j in range(6):
                    if ls[-(j+2)][i] == ".":
                        key.append(j)
                        break
            keys.append(tuple(key))

    res = 0
    for k in keys:
        for l in locks:
            if all(ki+kl < 6 for ki, kl in zip(k, l)):
                res += 1
    return res


def main():
    run_on_inputs(run, {1: 3})


if __name__ == "__main__":
    main()
