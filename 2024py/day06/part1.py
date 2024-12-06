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

def main():
    lb = ListBoard(lines)
    b = DictBoard(lb.to_dict(), ".")

    g = V(*b.find("^"))
    dir = Dir.N

    visited = set()

    while True:
        visited.add((g.tuple, dir.value.tuple))

        for _ in range(4):
            new = g + dir.value

            if b.board.get(new.tuple) == "#":
                dir = dir.turn(90)
                continue

        if new.tuple not in b.board:
            break

        if ((new.tuple, dir.value.tuple)) in visited:
            break

        g = new

    print(len({p for p, _ in visited}))

    
if __name__ == "__main__":
    main()
