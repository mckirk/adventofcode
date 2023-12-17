#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from dataclasses import dataclass, field

from lib import *


input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

LIMITS = (len(lines[0]), len(lines))
BOARD = {
    V(x, y): int(char) for y, line in enumerate(lines) for x, char in enumerate(line)
}
TARGET = V(len(lines[0]) - 1, len(lines) - 1)


@dataclass(frozen=True, unsafe_hash=True)
class State(AStarState):
    pos: V = V(0, 0)
    dir: V = None
    times_same_dir: int = 0
    total_heat_loss: int = field(default=0, hash=False, compare=False)

    def incurred_cost(self):
        return self.total_heat_loss

    def estimated_cost(self):
        return self.pos.dist(TARGET)

    def is_goal(self):
        return self.pos == TARGET and self.times_same_dir >= 4

    def next_states(self):
        if self.dir is None:
            pos_dirs = [V(1, 0), V(0, 1)]
        else:
            pos_dirs = []
            if self.times_same_dir >= 4:
                pos_dirs = [self.dir.turn(1), self.dir.turn(-1)]
            if self.times_same_dir < 10:
                pos_dirs.append(self.dir)

        for dir in pos_dirs:
            new_pos = self.pos + dir
            if not new_pos.within(LIMITS):
                continue

            new_times_same_dir = 1 if dir != self.dir else self.times_same_dir + 1
            new_state = State(
                new_pos, dir, new_times_same_dir, self.total_heat_loss + BOARD[new_pos]
            )

            yield new_state


def main():
    print(a_star([State()]).incurred_cost())


if __name__ == "__main__":
    main()
