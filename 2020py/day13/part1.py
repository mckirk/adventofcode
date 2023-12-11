#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
from aocparser import parse

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

spec = """\
{i}
[{w}|,]\
"""

def main():
    ts, ids = parse(spec, input)

    min_time = float("inf")
    min_id = None
    for id in ids:
        if id == "x":
            continue
        id = int(id)

        to_wait = id - (ts % id)
        if to_wait < min_time:
            min_time = to_wait
            min_id = id

    print(min_time, min_time*min_id)
    
if __name__ == "__main__":
    main()
