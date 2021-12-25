#!/usr/bin/env python3

import numpy as np

def fuel_for_pos(ns, pos):
    fuel = 0

    for n in ns:
        fuel += abs(n - pos)

    return fuel

def fuel_for_pos2(ns, pos):
    fuel = 0

    for n in ns:
        diff = abs(n - pos)
        fuel += int(diff*(diff+1) / 2)

    return fuel


def main():
    with open("./input.txt", "r") as f:
        inp = list(f.read().strip().splitlines())

    ns = [int(n) for n in inp[0].split(",")]

    avg = round(np.mean(ns))

    min_pos, min_fuel = None, None

    for i in range(max(ns)):
        fuel = fuel_for_pos(ns, i)

        if min_fuel is None or fuel < min_fuel:
            min_pos, min_fuel = i, fuel

    print(f"Puzzle 1: {min_fuel}")

    min_pos, min_fuel = None, None

    for i in range(max(ns)):
        fuel = fuel_for_pos2(ns, i)

        if min_fuel is None or fuel < min_fuel:
            min_pos, min_fuel = i, fuel

    print(f"Puzzle 2: {min_fuel}")

if __name__ == "__main__": main()