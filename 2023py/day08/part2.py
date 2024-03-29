#!/usr/bin/env python3
from pathlib import Path
import sys

sys.path.append(str(Path(__file__).parent.parent.parent))

from aocparser import parse
from lib.lcm import lcmm

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()

spec = """{w}

[{key:w} = ({L:w}, {R:w})|\n]"""

def main():
    d, nodes = parse(spec, input)

    start = set()

    for k in nodes.keys():
        if k[-1] == "A":
            start.add(k)

    cycles = set()

    for cur in start:
        steps = 0
        while cur [-1] != "Z":
            cur_d = d[steps % len(d)]
            cur_n = nodes[cur]
            cur = cur_n[cur_d]
            
            steps += 1

        cycles.add(steps)

    print(lcmm(*cycles))
    
if __name__ == "__main__":
    main()
