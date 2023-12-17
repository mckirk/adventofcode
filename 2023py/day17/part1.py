#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from dataclasses import dataclass, field

from lib import *


input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

TARGET = V(len(lines[0]) - 1, len(lines) - 1)


@dataclass(frozen=True, unsafe_hash=True)
class State:
    pos: V = V(0, 0)
    dir: V = None
    times_same_dir: int = field(default=0, hash=False, compare=False)
    total_heat_loss: int = field(default=0, hash=False, compare=False)

    def __lt__(self, other):
        my_f = self.pos.dist(TARGET) + self.total_heat_loss
        other_f = other.pos.dist(TARGET) + other.total_heat_loss
        return my_f < other_f


def main():
    board = dict()
    for y, line in enumerate(lines):
        for x, char in enumerate(line):
            board[V(x, y)] = int(char)

    start_state = State()

    to_do: PriorityQueue[State] = PriorityQueue()
    to_do.put(start_state)

    least_heat_loss: dict[State, list[tuple[int, int]]] = dict()
    target = V(len(lines[0]) - 1, len(lines) - 1)

    least_heat_loss_target = float("inf")

    least_heat_loss[start_state] = [(0, 0)]

    limits = (len(lines[0]), len(lines))

    while not to_do.empty():
        cur = to_do.get()

        if cur.pos == target:
            least_heat_loss_target = cur.total_heat_loss
            break

        if cur.dir is None:
            pos_dirs = [V(1, 0), V(0, 1)]
        else:
            pos_dirs = [cur.dir.turn(1), cur.dir.turn(-1)]
            if cur.times_same_dir < 3:
                pos_dirs.append(cur.dir)

        for dir in pos_dirs:
            new_pos = cur.pos + dir
            if not new_pos.within(limits):
                continue

            new_times_same_dir = 1 if dir != cur.dir else cur.times_same_dir + 1
            new_state = State(
                new_pos, dir, new_times_same_dir, cur.total_heat_loss + board[new_pos]
            )

            found_better = False
            heat_loss_by_times_same_dir = least_heat_loss.get(new_state)
            if heat_loss_by_times_same_dir is None:
                found_better = True
                least_heat_loss[new_state] = []
            else:
                for times_same_dir, heat_loss in heat_loss_by_times_same_dir:
                    if (
                        new_state.total_heat_loss < heat_loss
                        or new_times_same_dir < times_same_dir
                    ):
                        found_better = True
                        break

            if found_better:
                to_do.put(new_state)
                least_heat_loss[new_state].append(
                    (new_times_same_dir, new_state.total_heat_loss)
                )

    print(least_heat_loss_target)


if __name__ == "__main__":
    main()
