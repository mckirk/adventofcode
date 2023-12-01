#!/usr/bin/env python3
from pathlib import Path
import re

input_file = Path(__file__).parent / "input.txt"
input = input_file.read_text().strip()

def main():
    s = 0
    for line in input.split("\n"):
        nums = re.sub(r"\D", "", line)
        num = int(nums[0] + nums[-1])
        s += num
    print(s)
    
if __name__ == "__main__":
    main()
