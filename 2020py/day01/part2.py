#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    for l1 in lines:
        for l2 in lines:
            for l3 in lines:
                if int(l1) + int(l2) + int(l3) == 2020:
                    print(int(l1) * int(l2) * int(l3))
                    return
    
if __name__ == "__main__":
    main()
