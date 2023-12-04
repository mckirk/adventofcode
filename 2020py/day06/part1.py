#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    res = 0
    cur_answers = set()
    for line in lines + [""]:
        if not line:
            res += len(cur_answers)
            cur_answers = set()

        cur_answers |= set(line)

    print(res)
    
if __name__ == "__main__":
    main()
