#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def validate_height(s):
    if s.endswith("cm"):
        return 150 <= int(s[:-2]) <= 193
    elif s.endswith("in"):
        return 59 <= int(s[:-2]) <= 76
    else:
        return False

required = {
    "byr": lambda s: 1920 <= int(s) <= 2002,
    "iyr": lambda s: 2010 <= int(s) <= 2020,
    "eyr": lambda s: 2020 <= int(s) <= 2030,
    "hgt": validate_height,
    "hcl": lambda s: s[0] == "#" and len(s) == 7 and all(c in "0123456789abcdef" for c in s[1:]),
    "ecl": lambda s: s in {"amb", "blu", "brn", "gry", "grn", "hzl", "oth"},
    "pid": lambda s: len(s) == 9 and all(c in "0123456789" for c in s),
}

def main():
    valid_count = 0
    cur_passport = dict()
    for line in lines + [""]:
        if not line:
            valid = True
            for key, validator in required.items():
                try:
                    if not validator(cur_passport[key]):
                        valid = False
                except:
                    valid = False
            if valid:
                valid_count += 1

            cur_passport = dict()

        for part in line.split():
            key, val = part.split(":")
            cur_passport[key] = val

    print(valid_count)
        
    
if __name__ == "__main__":
    main()
