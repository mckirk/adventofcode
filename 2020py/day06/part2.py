#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    res = 0
    cur_answers = None
    for line in lines + [""]:
        if not line:
            # print(cur_answers)
            res += len(cur_answers)
            cur_answers = None
        else:
            if cur_answers is None:
                cur_answers = set(line)
            else:
                cur_answers = cur_answers.intersection(set(line))

    print(res)
    
if __name__ == "__main__":
    main()
