#!/usr/bin/env python3

from collections import deque
from lib import *


def state_to_int(state):
    r = 0
    while state:
        r <<= 1
        r += 1 if state[-1] == "#" else 0
        state = state[:-1]
    return r


def wiring_to_int(wiring):
    r = 0
    for n in wiring:
        r += 1 << n
    return r


def find_target(target, buttons):
    known = {0: 0}
    todo = deque()
    todo.append((0, 0))
    while todo:
        state, steps = todo.popleft()
        nsteps = steps+1
        for b in buttons:
            new = state ^ b
            if new == target:
                return nsteps
            
            if new not in known:
                known[new] = nsteps
                todo.append((new, nsteps))

    assert False


def run(inp: Input):
    res = 0
    for target_str, buttons_raw, joltage in inp.parsed:
        target = state_to_int(target_str)
        buttons = [wiring_to_int(b) for b in buttons_raw]
        res += find_target(target, buttons)
    return res


def main():
    run_on_inputs(run, {1: 7})


if __name__ == "__main__":
    main()
