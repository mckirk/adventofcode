#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from aocparser import parse

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
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

def print_boxes(boxes):
    for h, b in boxes.items():
        b_s = " ".join(f"[{k} {v}]" for k, v in b.items())
        print(f"Box {h}: {b_s}")

spec = "[<set:{n:w}={v:i}|minus:{n:w}->|,]"

def main():
    ins = parse(spec, input)
    boxes: defaultdict[int, dict] = defaultdict(dict)
    for i in ins:
        if i.set:
            h = hash1(i.n)
            boxes[h][i.n] = i.set.v
            # print(f"After {i.n}={i.set.v}:")
        elif i.minus:
            h = hash1(i.n)
            boxes[h].pop(i.n, None)
            # print(f"After {i.n}-:")
        # print_boxes(boxes)
        # print()

    res = 0
    for h, b in boxes.items():
        for i, v in enumerate(b.values(), start=1):
            res += (h+1)*i*v

    print(res)

    
if __name__ == "__main__":
    main()
