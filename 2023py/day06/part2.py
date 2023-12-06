#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    times = [int(lines[0].split(": ")[1].replace(" ", ""))]
    dists = [int(lines[1].split(": ")[1].replace(" ", ""))]

    print(times, dists)

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
