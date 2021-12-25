#!/usr/bin/env python3

import numpy as np

def simulate_for_raw(state, days):
    for _ in range(days):
        to_spawn = np.sum(state == 0)

        state[state == 0] = 7
        state -= 1

        state = np.append(state, [8]*to_spawn)

    return state

def simulate_for(state, days):
    for _ in range(days):
        to_spawn = state[0]

        state = np.roll(state, -1)
        state[6] += to_spawn

    return state



def puzzle1(state: np.ndarray):
    state = simulate_for(state, 80)

    print(f"Puzzle1: {sum(state)}")

    return state

def puzzle2(state: np.ndarray):
    state = simulate_for(state, 256-80)

    print(f"Puzzle2: {sum(state)}")

    return state

def transform_state(state_raw):
    state = np.zeros(9, dtype='uint64')

    for f in state_raw:
        state[f] += 1

    return state

def main():
    with open("./input.txt", "r") as f:
        inp = list(f.read().strip().splitlines())

    state = transform_state(int(f) for f in inp[0].split(","))

    state = puzzle1(state)
    puzzle2(state)

if __name__ == "__main__": main()