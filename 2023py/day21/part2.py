#!/usr/bin/env python3
from collections import defaultdict, Counter
from functools import cache
from pathlib import Path
from pprint import pprint
from aocparser import parse

from lib import *

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

DIRS = [Dir.N, Dir.S, Dir.E, Dir.W]
TILESET_DIRS = [V(0, 0)] + [d.value for d in DIRS]


def main():
    board = {V(x, y): c for y, line in enumerate(lines) for x, c in enumerate(line)}
    limits = (len(lines[0]), len(lines))
    vlimits = V(*limits)

    valid = {k for k, v in board.items() if v in "S."}

    start = [k for k, v in board.items() if v == "S"][0]

    seen = dict()

    pos_set_list = []
    pos_set_dict = dict()
    counts_by_idx = []

    def add_pos_set(pos_set):
        if not isinstance(pos_set, frozenset):
            pos_set = frozenset(pos_set)

        if pos_set in pos_set_dict:
            return pos_set_dict[pos_set]

        idx = len(pos_set_list)
        pos_set_list.append(pos_set)
        counts_by_idx.append(len(pos_set))
        pos_set_dict[pos_set] = idx
        return idx

    def simulate_tile_set(tile_set):
        new = set()
        to_do = set()

        for tile_dir, pos_set_idx in zip(TILESET_DIRS, tile_set):
            tile_pos = tile_dir.elem_mul(vlimits)
            if pos_set_idx is None:
                continue
            for pos in pos_set_list[pos_set_idx]:
                new_pos = pos + tile_pos
                to_do.add(new_pos)

        while to_do:
            cur = to_do.pop()
            for d in DIRS:
                p2 = cur + d.value

                if p2.tiled(limits) in valid and p2.tile(limits) == V(0, 0):
                    new.add(p2)

        if new:
            return add_pos_set(new)
        else:
            return None

    TARGET = 26501365
    assert TARGET % limits[0] == limits[0] // 2

    to_do = {V(0, 0): add_pos_set([start])}
    for steps in range(1, 1000):
        hit, miss = 0, 0
        next_to_do = dict()
        relevant_tiles = set(to_do.keys())
        for tile in to_do.keys():
            relevant_tiles.update(tile + d.value for d in DIRS)

        for tile in relevant_tiles:
            tile_set = tuple(to_do.get(tile + x) for x in TILESET_DIRS)
            cached = seen.get(tile_set, -1)
            if cached != -1:
                if cached is not None:
                    next_to_do[tile] = cached
                hit += 1
            else:
                simulated = simulate_tile_set(tile_set)
                seen[tile_set] = simulated
                if simulated:
                    next_to_do[tile] = simulated
                miss += 1

        to_do = next_to_do

        if (steps - (limits[0] // 2)) % limits[0] == 0:
            full_steps = steps // limits[0]
            print("#" * 80)
            print(steps, full_steps, hit / (hit + miss))
            tiles_by_count = Counter()
            for pos_set_idx in to_do.values():
                tiles_by_count[counts_by_idx[pos_set_idx]] += 1

            tiles_by_count = sorted(
                tiles_by_count.items(), key=lambda x: x[1], reverse=True
            )

            for pos_count, count in tiles_by_count:
                print(f"{pos_count}: {count}")

            pattern = (
                [full_steps**2, (full_steps - 1) ** 2]
                + [full_steps] * 4
                + [full_steps - 1] * 4
                + [1] * 4
            )

            if [count for _, count in tiles_by_count] == pattern:
                print("Pattern found!")
                break

        # if steps % 100 == 0:
        #     print(steps, sum(counts_by_idx[idx] for idx in to_do.values()), hit / (hit + miss))

    target_full_steps = TARGET // limits[0]
    target_pattern = (
        [target_full_steps**2, (target_full_steps - 1) ** 2]
        + [target_full_steps] * 4
        + [target_full_steps - 1] * 4
        + [1] * 4
    )

    res = sum(
        pos_count * p for (pos_count, _), p in zip(tiles_by_count, target_pattern)
    )
    print(res)


if __name__ == "__main__":
    import cProfile
    cProfile.run("main()")
