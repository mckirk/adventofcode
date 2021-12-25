#!/usr/bin/env python3

def count_incr(ns):
    inc = 0

    prev = None
    for n in ns:
        if prev and n > prev:
            inc += 1

        prev = n

    return inc


def main():
    with open("./input.txt", "r") as f:
        inp = f.read()

    nums = [int(l) for l in inp.strip().splitlines()]

    groups = [nums[i:i+3] for i in range(len(nums)-2)]
    sums = [sum(g) for g in groups if len(g) == 3]

    print(f"Puzzle 1: {count_incr(nums)})")
    print(f"Puzzle 2: {count_incr(sums)})")

if __name__ == "__main__": main()