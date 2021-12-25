#!/usr/bin/env python3

import numpy as np

def get_win_order(rands, boards):
    wins = dict()
    wins_order = []

    drawn = set()
    for r in rands:
        drawn.add(r)

        for i,b in enumerate(boards):
            board_idx = int(i / 2)

            for row in b:
                if set(row).issubset(drawn):
                    unmarked_sum = 0
                    for n in b.flatten():
                        if n not in drawn: unmarked_sum += n
                    
                    if not board_idx in wins.keys():
                        wins[board_idx]= r*unmarked_sum
                        wins_order.append(r*unmarked_sum)

    return wins_order


def main():
    with open("./input.txt", "r") as f:
        inp = list(f.read().strip().splitlines())

    rands = [int(n) for n in inp[0].split(",")]
    boards: list[np.ndarray] = []

    for i in range(2, len(inp), 6):
        board = np.array([[int(n) for n in l.split()] for l in inp[i:i+5]], dtype=int)
        boards.append(board)
        boards.append(np.transpose(board))

    win_order = get_win_order(rands, boards)

    print(f"Puzzle 1: {win_order[0]}")
    print(f"Puzzle 2: {win_order[-1]}")


if __name__ == "__main__": main()