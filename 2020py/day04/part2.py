#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

required = set([
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid",
    # "cid",
])

def main():
    valid = 0
    cur_passport = set()
    for line in lines + []:
        if not line:
            print(cur_passport)
            if cur_passport.intersection(required) == required:
                valid += 1
            cur_passport = set()

        for part in line.split():
            cur_passport.add(part.split(":")[0])

    print(valid)
        
    
if __name__ == "__main__":
    main()
