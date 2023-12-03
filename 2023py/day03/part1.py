#!/usr/bin/env python3
from pathlib import Path
import re

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    nums_with_pos = []
    symbols_with_pos = []
    for y, line in enumerate(lines):
        x = 0
        while x < len(line):
            num_match = re.match(r"\d+", line[x:])
            if num_match:
                num = num_match.group()
                nums_with_pos.append((num, x, y))
                x += len(num)
            elif line[x] == ".":
                x += 1
            else:
                symbols_with_pos.append((line[x], x, y))
                x += 1
    
    res = 0
    for num, x, y in nums_with_pos:
        for symbol, x2, y2 in symbols_with_pos:
            # any adjacent symbol is valid
            if x2 in range(x-1, x+len(num)+1) and abs(y - y2) <= 1:
                res += int(num)
                break

    print(res)
    
if __name__ == "__main__":
    main()
