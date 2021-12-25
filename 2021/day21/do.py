#!/usr/bin/env python3

import numpy as np
from collections import defaultdict
import re

from scipy import signal

def main():
    with open("./input.txt", "r") as f:
       inp = list(f.read().strip().splitlines())
    # inp = toy_input.splitlines()

    player1_pos, player2_pos = 4, 8
    player1_score, player2_score = 0, 0

    times, times_total = 0, 0
    def roll():
        nonlocal times, times_total
        times += 1
        times_total += 1

        times = (times - 1) % 100 + 1
        return times

    while True:
        for i in range(3): player1_pos += roll()
        player1_pos = (player1_pos - 1) % 10 + 1
        player1_score += player1_pos
        print(f"Player1 score: {player1_score}")
        if player1_score >= 1000:
            print(f"Puzzle1: {player2_score*times_total}")
            break

        for i in range(3): player2_pos += roll()
        player2_pos = (player2_pos - 1) % 10 + 1

        player2_score += player2_pos
        print(f"Player2 score: {player2_score}")
        if player2_score >= 1000:
            print(f"Puzzle1: {player1_score*times_total}")
            break

        pass

    pass

if __name__ == "__main__": main()