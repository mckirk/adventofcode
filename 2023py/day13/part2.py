#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from abc import ABC, abstractmethod
from typing import Iterable
import colored


class Board(ABC):
    def pprint(self, colors: dict[tuple, str]):
        ls = []
        for y, l in enumerate(self.by_lines()):
            cur_l = ""
            for x, c in enumerate(l):
                color = colors.get((x, y))
                if color:
                    cur_l += f"{colored.fore(color)}{c}{colored.Style.reset}"
                else:
                    cur_l += c
            ls.append(cur_l)

        print("\n".join(ls))

    @abstractmethod
    def by_lines(self) -> Iterable:
        ...


def num_diff(s1, s2):
    return sum(1 for x, y in zip(s1, s2) if x != y)


class ListBoard(Board):
    def __init__(self, board: list[str]):
        self.board = board
        self.limits = (len(board[0]), len(board))

    def by_lines(self):
        return self.board

    def to_dict(self):
        tiles = dict()
        for y, l in enumerate(self.board):
            for x, c in enumerate(l):
                tiles[(x, y)] = c

        return tiles
    
    def find_reflection_y(self, num_defects=0):
        for i, _ in enumerate(self.board):
            parts = self.board[:i][::-1], self.board[i:]
            min_len = min(len(p) for p in parts)
            if not min_len:
                continue
            parts = [p[:min_len] for p in parts]
            if num_defects:
                total_num_diff = sum(num_diff(*p) for p in zip(*parts))
                if total_num_diff == num_defects:
                    return i
            else:
                if parts[0] == parts[1]:
                    return i
        return None


def transpose(lines):
    return ["".join(l) for l in zip(*lines)]


input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

def main():
    res = 0
    for b in blocks:
        lines = b.splitlines()
        board = ListBoard(lines)
        board_transp = ListBoard(transpose(lines))

        reflection_y = board.find_reflection_y(1)
        if reflection_y:
            res += 100*reflection_y
        else:
            reflection_x = board_transp.find_reflection_y(1)
            assert reflection_x
            res += reflection_x

    print(res)

    
if __name__ == "__main__":
    main()
