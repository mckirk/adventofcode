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

def main():
    ns = [int(n) for n in input.split(",")]

    times_spoken = defaultdict(list)
    last_spoken = None

    def speak(i, n):
        nonlocal last_spoken
        times_spoken[n].append(i)
        last_spoken = n
    
    for i in range(2020):
        if i < len(ns):
            speak(i, ns[i])
            continue

        times = times_spoken[last_spoken]
        if len(times) == 1:
            speak(i, 0)
        else:
            speak(i, times[-1] - times[-2])

    print(last_spoken)

    
    
if __name__ == "__main__":
    main()
