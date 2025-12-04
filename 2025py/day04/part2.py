#!/usr/bin/env python3

from lib import *


def run(inp: Input):
    rolls = {p for p, c in inp.as_pos if c == "@"}
    res = 0
    while True:
        to_remove = []
        for p in rolls:
            cnt = 0
            for _ in p.adj(valid=rolls, diagonal=True):
                cnt += 1
                if cnt >= 4: break
            else: to_remove.append(p)
        if not to_remove:
            break
        for p in to_remove:
            rolls.remove(p)
            res += 1

    return res


def main():
    run_on_inputs(run, {1: 43})


if __name__ == "__main__":
    main()
