#!/usr/bin/env python3
from collections import defaultdict
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    instances_of = defaultdict(lambda: 0)
    stack = list(range(len(lines)))
    while stack:
        i = stack.pop()
        if i >= len(lines):
            continue
        instances_of[i] += 1
        line = lines[i]
        parts1 = line.split(": ")
        parts2 = parts1[1].split(" | ")
        nums_want = parts2[0].split()
        nums_have = parts2[1].split()

        overlap = set(nums_want).intersection(set(nums_have))
        if overlap:
            score = len(overlap)
            for j in range(score):
                new_idx = i+j+1
                stack.append(new_idx)
    print(sum(instances_of.values()))
    
if __name__ == "__main__":
    main()
