#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    d = lines[0]

    nodes = dict()
    for l in lines[2:]:
        p1, p2 = l.split(" = ")
        p2_1, p2_2 = p2.split(", ")

        nodes[p1] = [p2_1[1:], p2_2[:-1]]

    cur = "AAA"
    steps = 0

    while cur != "ZZZ":
        cur_n = nodes[cur]
        cur_d = d[steps % len(d)]
        if cur_d == "L":
            cur = cur_n[0]
        elif cur_d == "R":
            cur = cur_n[1]
        
        steps += 1

    print(steps)
    
if __name__ == "__main__":
    main()
