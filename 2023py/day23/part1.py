#!/usr/bin/env python3
from collections import defaultdict, Counter
from dataclasses import field
from pathlib import Path
from pprint import pprint
from aocparser import parse

from lib import *

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")


@dataclass(frozen=True)
class State:
    cur_pos: V
    seen: set[V] = field(default_factory=set)
    path_length: int = 0

    def __hash__(self):
        return hash((self.cur_pos, self.path_length, frozenset(self.seen)))

    def next_state(self, pos, limits):
        if pos.within(limits) and not pos in self.seen:
            return State(pos, self.seen | {pos}, self.path_length + 1)
        else:
            return None
        
    def add_to_do(self, pos, limits, to_do):
        next_state = self.next_state(pos, limits)
        if next_state is not None:
            to_do.append(next_state)
        return to_do


SLOPES = {
    ">": V(1, 0),
    "<": V(-1, 0),
    "^": V(0, -1),
    "v": V(0, 1),
}

DIRS = [V(1, 0), V(-1, 0), V(0, -1), V(0, 1)]


def main():
    board = {V(x, y): c for y, line in enumerate(lines) for x, c in enumerate(line)}
    limits = (len(lines[0]), len(lines))
    start = [V(x, 0) for x, c in enumerate(lines[0]) if c == "."][0]
    end = [V(x, len(lines) - 1) for x, c in enumerate(lines[-1]) if c == "."][0]

    longest = dict()
    to_do = [State(start)]

    while to_do:
        next_to_do = []

        for s in to_do:
            longest[s.cur_pos] = max(longest.get(s.cur_pos, 0), s.path_length)

            b = board[s.cur_pos]
            slope = SLOPES.get(b)
            if slope is not None:
                s.add_to_do(s.cur_pos + slope, limits, next_to_do)
                continue

            for d in DIRS:
                n = s.cur_pos + d
                if board.get(n) != "#":
                    s.add_to_do(s.cur_pos + d, limits, next_to_do)

        to_do = next_to_do

    print(longest[end])


    
if __name__ == "__main__":
    main()
