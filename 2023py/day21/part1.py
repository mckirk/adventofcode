#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from aocparser import parse

from lib import *

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

DIRS = [Dir.N, Dir.S, Dir.E, Dir.W]

def main():
    board = {V(x, y): c for y, line in enumerate(lines) for x, c in enumerate(line)}

    start = [k for k, v in board.items() if v == "S"][0]

    seen = {start}
    to_do = {start}
    for i in range(64):
        next_to_do = set()
        while to_do:
            cur = to_do.pop()
            for d in DIRS:
                p2 = cur + d.value
                if board[p2] == ".":
                    seen.add(p2)
                    next_to_do.add(p2)
        to_do = next_to_do

    print(len(to_do)+1)
    
if __name__ == "__main__":
    main()
