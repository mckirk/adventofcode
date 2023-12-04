#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    trees = 0
    for i, line in enumerate(lines):
        if line[(i*3) % len(line)] == "#":
            trees += 1
    print(trees)
    
if __name__ == "__main__":
    main()
