#!/usr/bin/env python3
from pathlib import Path
from aocparser import parse


input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()

spec = """{w}

[{key:w} = ({L:w}, {R:w})|\n]"""

def main():
    d, nodes = parse(spec, input)

    cur = "AAA"
    steps = 0

    while cur != "ZZZ":
        cur_n = nodes[cur]
        cur_d = d[steps % len(d)]
        cur = cur_n[cur_d]
        
        steps += 1

    print(steps)
    
if __name__ == "__main__":
    main()
