#!/usr/bin/env python3

from lib import *


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
        if found:
            res += 1

    return res


def main():
    run_on_inputs(run, {1: 6})


if __name__ == "__main__":
    main()
