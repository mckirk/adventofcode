#!/usr/bin/env python3

import numpy as np
from collections import defaultdict
import re

toy_input = """NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"""

def subst_for(template, produces, rule_dict, times):
    part_counts = defaultdict(lambda: 0)
    for i in range(len(template)-1):
        part_counts[template[i:i+2]] += 1

    letter_counts = defaultdict(lambda: 0)
    for c in template:
        letter_counts[c] += 1

    for i in range(times):
        new_counts = defaultdict(lambda: 0)

        for p,c in part_counts.items():
            for produced in produces[p]:
                new_counts[produced] += c

            if p in rule_dict.keys():
                letter_counts[rule_dict[p]] += c

        part_counts = new_counts

    sorted_letters = list(letter_counts.items())
    sorted_letters.sort(key = lambda x: x[1])

    return sorted_letters

def main():
    with open("./input.txt", "r") as f:
       inp = list(f.read().strip().splitlines())
    # inp = toy_input.splitlines()

    template = inp[0]

    rules = []
    for l in inp[2:]:
        left, right = l.split(" -> ")
        # right = f"{left[0]}{right[0]}{left[1]}"
        rules.append((left, right))

    rule_dict = dict(rules)
    produces = dict()

    import itertools, string
    for a,b in itertools.product(string.ascii_uppercase, string.ascii_uppercase):
        produces[a+b] = [a+b]

    for k in rule_dict.keys():
        ins = rule_dict[k]
        produces[k] = [k[0] + ins, ins + k[1]]

    sorted_letters1 = subst_for(template, produces, rule_dict, 10)
    print(f"Puzzle1: {sorted_letters1[-1][1] - sorted_letters1[0][1]}")

    sorted_letters2 = subst_for(template, produces, rule_dict, 40)
    print(f"Puzzle2: {sorted_letters2[-1][1] - sorted_letters2[0][1]}")

    pass

if __name__ == "__main__": main()