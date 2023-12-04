#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    res = 1
    for (x_del, y_del) in [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]:
        trees = 0
        i = 0
        while True:
            x = i*x_del
            y = i*y_del

            print(f"{i}: {x}, {y}")
            if y >= len(lines):
                break

            line = lines[y]

            if line[x % len(line)] == "#":
                trees += 1
            i += 1
        print(trees)
        res *= trees
    print(res)
    
if __name__ == "__main__":
    main()
