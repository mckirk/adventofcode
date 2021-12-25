#!/usr/bin/env python3

import numpy as np
from collections import defaultdict
import re

toy_input = """1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"""

class Cell:
    def __init__(self, risk, pos):
        self.risk = risk
        self.pos = pos

        self.lowest_risk = None
        self.lowest_from = None

    def visit_from(self, from_cell):
        new_risk = from_cell.lowest_risk + self.risk

        unexplored = self.lowest_risk is None

        if unexplored or self.lowest_risk > new_risk:
            self.lowest_risk = new_risk
            self.lowest_from = from_cell
            return True

        return False

def find_path(cells: "list[list[Cell]]"):
    start = cells[0][0]
    start.lowest_risk = 0
    start.lowest_from = start

    to_explore = [start]
    to_explore_idx = 0

    while to_explore_idx < len(to_explore):
        explore_cell = to_explore[to_explore_idx]
        to_explore_idx += 1
        
        explore_x, explore_y = explore_cell.pos

        for (xd, yd) in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            neighbor_x, neighbor_y = explore_x + xd, explore_y + yd

            if neighbor_x >= 0 and neighbor_x < len(cells[0]):
                if neighbor_y >= 0 and neighbor_y < len(cells):
                    neighbor = cells[neighbor_y][neighbor_x]
                    if neighbor.visit_from(explore_cell):
                        to_explore.append(neighbor)

    return cells[-1][-1].lowest_risk

def enlarge(cells: "list[list[Cell]]"):
    dim_x, dim_y = len(cells[0]), len(cells)

    new_cells = [[None for x in range(dim_x*5)] for y in range(dim_y*5)]

    for y in range(dim_y*5):
        for x in range(dim_x*5):
            ym = y % dim_y
            xm = x % dim_x

            prev_risk = cells[ym][xm].risk

            import math
            yf = math.floor(y / dim_y)
            xf = math.floor(x / dim_x)

            new_risk = ((prev_risk + xf + yf - 1) % 9) + 1

            new_cells[y][x] = Cell(new_risk, (x,y))

    return new_cells


def main():
    with open("./input.txt", "r") as f:
       inp = list(f.read().strip().splitlines())
    # inp = toy_input.splitlines()

    cells = [[Cell(int(n), (x,y)) for x,n in enumerate(l)] for y,l in enumerate(inp)]

    print(f"Puzzle1: {find_path(cells)}")

    large_cells = enlarge(cells)

    print(f"Puzzle2: {find_path(large_cells)}")

    pass

if __name__ == "__main__": main()