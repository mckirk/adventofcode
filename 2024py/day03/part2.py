#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from aocparser import parse

from lib import *

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

def main():
    import re
    res = 0
    e = True
    for m in re.findall(r"(mul)\((\d+),(\d+)\)|(do)\(\)|(don't)\(\)", input):
        if m[3] == "do":
            e = True
        if m[4] == "don't":
            e = False
        if m[0] == "mul" and e:
            res += int(m[1])*int(m[2])
    print(res)
    
if __name__ == "__main__":
    main()
