#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from aocparser import parse

from lib import *

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

def main():
    import re
    res = 0
    for m in re.findall(r"mul\((\d+),(\d+)\)", input):
        res += int(m[0])*int(m[1])
    print(res)
    
if __name__ == "__main__":
    main()
