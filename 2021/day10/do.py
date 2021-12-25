#!/usr/bin/env python3

import numpy as np

pairs = {'(': ')', '[': ']', '{': '}', '<': '>'}
scores_illegal = {
    ')': 3,
    ']': 57,
    '}': 1197,
    '>': 25137}
scores_fix = {
    ')': 1,
    ']': 2,
    '}': 3,
    '>': 4}


def find_illegal(l):
    stack = []

    for c in l:
        if c in pairs.keys():
            stack.append(pairs[c])
        else:
            if stack[-1] != c:
                return (c, stack)
            else:
                stack = stack[:-1]

    return (None, stack)

def score_fix(fix):
    score = 0
    for c in fix:
        score *= 5
        score += scores_fix[c]

    return score


def main():
    with open("./input.txt", "r") as f:
        inp = list(f.read().strip().splitlines())
    # inp = toy_input.splitlines()

    score_illegal = 0
    scores_fix = []

    for l in inp:
        illegal, stack = find_illegal(l)

        if illegal:
            score_illegal += scores_illegal[illegal]
        else:
            fix = "".join(reversed(stack))
            scores_fix.append(score_fix(fix))


    print(f"Puzzle1: {score_illegal}")
    print(f"Puzzle2: {np.median(scores_fix)}")

    pass

if __name__ == "__main__": main()