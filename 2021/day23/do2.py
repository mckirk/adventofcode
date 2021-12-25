#!/usr/bin/env python3

from typing import Tuple
import numpy as np
from collections import defaultdict
import re

from scipy import signal

from functools import reduce

toy_input = """#############
#...........#
###B#C#B#D###
  #D#C#B#A#
  #D#B#A#C#
  #A#D#C#A#
  #########"""

pattern = """#############
#(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)#
###(.)#(.)#(.)#(.)###
  #(.)#(.)#(.)#(.)#
  #(.)#(.)#(.)#(.)#
  #(.)#(.)#(.)#(.)#
  #########"""

template = """#############
#...........#
###.#.#.#.###
  #.#.#.#.#
  #.#.#.#.#
  #.#.#.#.#
  #########""".replace(".", "{}")


rooms_pos = [[(x,y) for y in [1,2,3,4]] for x in [2,4,6,8]]
hallway_pos = [(i, 0) for i in range(11)]

start_pos = [(x,y) for y in [1,2,3,4] for x in [2,4,6,8]]

pos_in_order = hallway_pos + start_pos

rooms_set = [frozenset(rs) for rs in rooms_pos]
room_set = reduce(frozenset.__or__, rooms_set, frozenset())
hallway_set = frozenset(hallway_pos)

possible_set = room_set | hallway_set

cant_stay_set = frozenset([(2, 0), (4, 0), (6, 0), (8, 0)])
hallway_dest_set = hallway_set - cant_stay_set

possible_deltas = [(1, 0), (-1, 0), (0, 1), (0, -1)]

transl = {'A': 0, 'B': 1, 'C': 2, 'D': 3}
cost_lookup = {0: 1, 1: 10, 2: 100, 3: 1000}

class AnthroConfig:
    def __init__(self, positions: frozenset):
        self.positions = positions

        self.occupied = frozenset(pos for (_, pos) in positions)
        self.free = possible_set - self.occupied

        self.hallway_pieces = [p for p in positions if p[1] in hallway_set]
        self.rooms_pieces = [[p for p in positions if p[1] in s] for s in rooms_set]

    @staticmethod
    def from_set(positions):
        return AnthroConfig(positions)

    @staticmethod
    def parse(inp):
        parsed = [c for c in re.match(pattern, inp).groups()]

        parsed_with_pos = [(transl[c], pos) for (c, pos) in zip(parsed, pos_in_order) if c != "."]

        return AnthroConfig.from_set(frozenset(parsed_with_pos))

    def calc_dist(self, f, t):
        fx, fy = f
        tx, ty = t

        intermediate = set()

        if fx != tx:
            for x in range(min(fx, tx), max(fx, tx) + 1):
                intermediate.add((x,0))
            for y in range(0, fy):
                intermediate.add((fx, y))
            for y in range(0, ty+1):
                intermediate.add((tx, y))
        else:
            for y in range(min(fy, ty), max(fy, ty)+1):
                intermediate.add((fx, y))

        intermediate.discard(f)

        if intermediate & self.occupied: return None

        return len(intermediate)

    def possible_resulting_configs(self):
        possible_configs = []
        possible_moves = []

        free_room_pos = []
        for piece,room in enumerate(self.rooms_pieces):
            if any(p != piece for (p,_) in room):
                free_room_pos.append(set())
                continue

            free_room_pos.append(rooms_set[piece] - frozenset(pos for (_,pos) in room))

        assert len(free_room_pos) == 4

        for (piece, piece_pos) in self.hallway_pieces:
            (px, py) = piece_pos

            for frp in free_room_pos[piece]:
                possible_moves.append((piece, piece_pos, frp))

        free_hallway = self.free & hallway_dest_set

        for room in self.rooms_pieces:
            for (piece, piece_pos) in room:
                for fh in free_hallway:
                    possible_moves.append((piece, piece_pos, fh))
                for fr in free_room_pos[piece]:
                    possible_moves.append((piece, piece_pos, fr))

        for piece, old_pos, new_pos in possible_moves:
            dist = self.calc_dist(old_pos, new_pos)
            if not dist:
                continue

            cost = cost_lookup[piece] * dist

            new_pos = (self.positions - frozenset([(piece, old_pos)])) | frozenset([(piece, new_pos)])

            possible_configs.append((AnthroConfig(new_pos), cost))

        return possible_configs

    def __eq__(self, other):
        return self.positions == other.positions

    def __hash__(self):
        return hash(self.positions)

    def __str__(self):
        piece_by_pos = {pos: "." for pos in possible_set}
        piece_by_pos.update({pos: "ABCD"[p] for (p, pos) in self.positions})

        return template.format(*(piece_by_pos[pos] for pos in pos_in_order))

target_config = AnthroConfig.from_set(frozenset(zip([0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3], start_pos)))


def energy_required(config: AnthroConfig, target: AnthroConfig):
    cur_states = {config: 0}

    # visited = set([config])
    least_cost = {config: 0}

    results = []

    step = 0
    while cur_states:
        new_states = dict()

        for (s, cost) in cur_states.items():
            if s == target:
                print(f"Have res: {cost}")
                results.append(cost)
            else:
                for (pc,step_cost) in s.possible_resulting_configs():
                    # print(str(pc))
                    # print()

                    total_cost = cost+step_cost
                    least_cost_so_far = least_cost.get(pc)
                    if least_cost_so_far is None or total_cost < least_cost_so_far:
                        least_cost[pc] = total_cost
                        new_states[pc] = total_cost

        cur_states = new_states

        step += 1
        print(f"Step {step}: {len(cur_states)} states")

    return min(results)

def main():
    with open("./input2.txt", "r") as f:
       inp = f.read().strip()
    # inp = toy_input

    init_conf = AnthroConfig.parse(inp)

    print(f"Puzzle2: {energy_required(init_conf, target_config)}")

    pass

if __name__ == "__main__": main()