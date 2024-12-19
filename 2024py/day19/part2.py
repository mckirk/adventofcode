#!/usr/bin/env python3

from functools import cache
from lib import *


def find_ways(patterns, design):
    @cache
    def find_from(i):
        if i >= len(design):
            return 1

        s = 0
        for p in patterns:
            if design[i:].startswith(p):
                s += find_from(i + len(p))

        return s

    return find_from(0)


def run(inp: Input):
    patterns, designs = inp.parsed

    res = 0
    for d in designs:
        found = False
        states = set([0])
        while states:
            new_states = set()
            for s in states:
                for p in patterns:
                    if d[s:].startswith(p):
                        ns = s + len(p)
                        if ns == len(d):
                            found = True
                            break
                        new_states.add(ns)
                if found:
                    break
            if found:
                break
            states = new_states
        if not found:
            continue

        res += find_ways(patterns, d)

    return res


def main():
    run_on_inputs(run, {1: 16})


if __name__ == "__main__":
    main()
