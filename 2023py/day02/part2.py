#!/usr/bin/env python3
from itertools import product
from pathlib import Path
import re

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()

def parse_line(line):
    # Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red

    parts = line.split(": ")
    game = int(parts[0].split(" ")[1])
    subsets = parts[1].split("; ")
    game_cubes = []
    for subset in subsets:
        cube_cur = subset.split(", ")
        cube = {}
        for c in cube_cur:
            count, color = c.split(" ")
            cube[color] = int(count)
        game_cubes.append(cube)
    return game, game_cubes


def possible(game_cubes, cube_count):
    return all(all([cube[color] <= cube_count[color] for color in cube]) for cube in game_cubes)


def minimum(game_cubes):
    min_count = {"red": 0, "green": 0, "blue": 0}
    for cube in game_cubes:
        for color in cube:
            min_count[color] = max(min_count[color], cube[color])
    return min_count

def power_count(min_count):
    res = 1
    for color in min_count:
        res *= min_count[color]
    return res


def main():
    games = [parse_line(line) for line in input.splitlines()]
    res = 0
    for game, game_cubes in games:
        min_count = minimum(game_cubes)
        print(f"Game {game}: {min_count}, {power_count(min_count)}")
        res += power_count(min_count)
    print(f"Result: {res}")
    
if __name__ == "__main__":
    main()
