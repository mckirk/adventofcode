#!/usr/bin/env python3

import numpy as np
from collections import defaultdict
import re

from scipy import signal

def split_factor():
    factors = defaultdict(lambda: 0)
    for d1 in range(1, 4):
        for d2 in range(1, 4):
            for d3 in range(1, 4):
                factors[d1+d2+d3] += 1

    return factors

def main():
    with open("./input.txt", "r") as f:
       inp = list(f.read().strip().splitlines())
    # inp = toy_input.splitlines()

    state = {(4, 8, 0, 0): 1}

    player1_won = 0
    player2_won = 0

    factors = split_factor()

    while state:
        new_state = defaultdict(lambda: 0)
        for (pos1,pos2,score1,score2),times in state.items():
            for p1_die in range(3, 10):
                player1_factors = factors[p1_die]

                new_pos1 = (pos1 + p1_die - 1) % 10 + 1
                new_score1 = score1 + new_pos1

                if new_score1 >= 21:
                    player1_won += times * player1_factors
                    continue

                for p2_die in range(3, 10):
                    player2_factors = factors[p2_die]

                    new_pos2 = (pos2 + p2_die - 1) % 10 + 1
                    new_score2 = score2 + new_pos2

                    if new_score2 >= 21:
                        player2_won += times * player1_factors * player2_factors
                        continue

                    new_state[(new_pos1, new_pos2, new_score1, new_score2)] += times * player1_factors * player2_factors

        state = new_state

    print(player1_won, player2_won)

    print(f"Larger: {max(player1_won, player2_won)}")

    pass

if __name__ == "__main__": main()