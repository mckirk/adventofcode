#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    valid_count = 0
    for line in lines:
        parts = line.split()
        min, max = parts[0].split("-")
        min = int(min)
        max = int(max)
        letter = parts[1][0]
        password = parts[2]
        if (password[min-1] == letter) ^ (password[max-1] == letter):
            valid_count += 1
    print(valid_count)
    
if __name__ == "__main__":
    main()
