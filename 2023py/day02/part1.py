#!/usr/bin/env python3
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


def main():
    games = [parse_line(line) for line in input.splitlines()]
    cube_count = {"red": 12, "green": 13, "blue": 14}
    possible_ids = 0
    for game, game_cubes in games:
        if possible(game_cubes, cube_count):
            possible_ids += game
            print(f"Possible: {game_cubes}")
        else:
            print(f"Impossible: {game_cubes}")
    print(f"Possible ID count: {possible_ids}")
    
if __name__ == "__main__":
    main()
