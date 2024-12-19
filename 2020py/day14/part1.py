#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from aocparser import parse

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

spec = "[<mask:mask = {w}|mem:mem\\[{i}\\] = {i}>|\n]"

def main():
    ins = parse(spec, input)

    mask_mask = None
    mask_val = None
    mem = defaultdict(int)
    for i in ins:
        if i.mask:
            mask_mask = int(i.mask.replace("X", "1"), 2)
            mask_val = int(i.mask.replace("X", "0"), 2)
        elif i.mem:
            mem[i.mem[0]] = i.mem[1] & mask_mask | mask_val

    print(sum(mem.values()))
    
if __name__ == "__main__":
    main()
