#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    res = 0
    for line in lines:
        parts1 = line.split(": ")
        parts2 = parts1[1].split(" | ")
        nums_want = parts2[0].split()
        nums_have = parts2[1].split()

        overlap = set(nums_want).intersection(set(nums_have))
        if overlap:
            score = 2**(len(overlap)-1)
            print(f"{line} -> {overlap} -> {score}")
            res += score
    print(res)
    
if __name__ == "__main__":
    main()
