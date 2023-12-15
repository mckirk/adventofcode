#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from aocparser import parse

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

def hash1(s):
    res = 0
    for c in s:
        res += ord(c)
        res *= 17
        res %= 256
    return res

def main():
    ins = input.split(",")

    res = 0
    for i in ins:
        res += hash1(i)
    print(res)
    
if __name__ == "__main__":
    main()
