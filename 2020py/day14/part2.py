#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from aocparser import parse

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

spec = "[<mask:mask = {w}|mem:mem`[{i}`] = {i}>|\n]"

def gen_addresses(addr, mask):
    ones = int(mask.replace("X", "0"), 2)
    addr |= ones

    floating = [2 ** i for i, c in enumerate(mask[::-1]) if c == "X"]
    for i in range(2 ** len(floating)):
        new_addr = addr
        for j, f in enumerate(floating):
            if i & (2 ** j):
                new_addr |= f
            else:
                new_addr &= ~f
        yield new_addr

def main():
    ins = parse(spec, input)

    mask = None
    mem = defaultdict(int)
    for i in ins:
        if i.mask:
            mask = i.mask
        elif i.mem:
            for addr in gen_addresses(i.mem[0], mask):
                # print(f"wrote {i.mem[1]} to {addr}")
                mem[addr] = i.mem[1]

    print(sum(mem.values()))
    
if __name__ == "__main__":
    main()
