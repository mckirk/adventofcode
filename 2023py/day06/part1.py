#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    times = [int(x) for x in lines[0].split(": ")[1].split()]
    dists = [int(x) for x in lines[1].split(": ")[1].split()]

    res = 1
    for t, d in zip(times, dists):
        ways = 0
        for i in range(t):
            dist = (t - i) * i
            if dist > d:
                ways += 1
        res *= ways
    print(res)
    
if __name__ == "__main__":
    main()
