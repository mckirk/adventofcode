#!/usr/bin/env python3

import numpy as np
import re

class Line:
    def __init__(self, l: str):
        split = re.split(r",| -> ", l)
        self.x1, self.y1, self.x2, self.y2 = [int(n) for n in split]

    def max_x(self):
        return max(self.x1, self.x2)

    def max_y(self):
        return max(self.y1, self.y2)

    def is_straight(self):
        return self.x1 == self.x2 or self.y1 == self.y2

    def get_delta(self):
        return (self.x2 - self.x1), (self.y2 - self.y1)

    def taxicab_len(self):
        xd, yd = self.get_delta()
        return abs(xd) + abs(yd)

    def add_fields(self, board):
        xd, yd = self.get_delta()
        xdir, ydir = np.sign(xd), np.sign(yd)

        x, y = self.x1, self.y1
        while True:
            board[x,y] += 1
            x += xdir
            y += ydir

            if (x,y) == (self.x2, self.y2):
                break

        board[x,y] += 1

def main():
    with open("./input.txt", "r") as f:
        inp = list(f.read().strip().splitlines())

    lines = [Line(l) for l in inp]

    max_x = max(l.max_x() for l in lines)
    max_y = max(l.max_y() for l in lines)

    board = np.zeros((max_x+1, max_y+1), dtype='int64')

    for l in lines:
        if not l.is_straight(): continue
        l.add_fields(board)

    print(f"Puzzle1: {np.sum(board > 1)}")

    board = np.zeros((max_x+1, max_y+1), dtype='int64')

    for l in lines:
        l.add_fields(board)

    print(f"Puzzle2: {np.sum(board > 1)}")


if __name__ == "__main__": main()