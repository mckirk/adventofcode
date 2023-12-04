#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    missing = set()
    for line in lines:
        missing.add(2020 - int(line))
    for line in lines:
        if int(line) in missing:
            print(int(line) * (2020 - int(line)))
            break
    
if __name__ == "__main__":
    main()
