#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    max_id = 0
    for line in lines:
        part1 = line[0:7]
        part2 = line[7:]

        row = int(part1.replace("F", "0").replace("B", "1"), 2)
        col = int(part2.replace("L", "0").replace("R", "1"), 2)

        # print(f"{line} -> {row} -> {col}")

        seat_id = row * 8 + col
        max_id = max(seat_id, max_id)

    print(max_id)
    
if __name__ == "__main__":
    main()
